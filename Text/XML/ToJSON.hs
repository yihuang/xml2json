{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Text.XML.ToJSON
  (
{-| This library provide a way to convert xml to json.
    
    Further more, by combining with aeson's parsing facility, it provide a way to parse xml to haskell data type.
 -}
    xmlToJSON
  , parseXML
  , JSONParseError(JSONParseError)
    -- * utils
  , tokensToJSON
  , elementToJSON
  , tokensToElement
  , tokenToBuilder
  ) where

import Control.Monad (when, liftM)
import Control.Arrow (second)
import Control.Exception (Exception)

import Data.Typeable (Typeable)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import Data.Conduit (($=), ($$), MonadThrow(monadThrow))
import qualified Data.Conduit.List as C
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Text.HTML.TagStream
import qualified Text.HTML.TagStream.Text as T
import Text.XML.ToJSON.Builder
import Data.Aeson (Value(..), Object, FromJSON, fromJSON, Result(Error, Success))

-- | Convert tagstream-conduit `Token' to xml element `Builder'
tokenToBuilder :: T.Token -> Builder
tokenToBuilder (TagOpen s as selfClose) = do
    beginElement s
    addAttrs as
    when selfClose endElement
tokenToBuilder (TagClose _) = endElement -- FIXME should match tag name?
tokenToBuilder (Text s) = addValue s
tokenToBuilder _ = return ()

-- |Convert xml `Element' to aeson `Value' .
--
-- xml attributes and text values are converted to special object attribute @__attributes@ and @__values@.
elementToJSON :: Element -> Value
elementToJSON (Element as vs cs) =
    if null as && null cs
      then
        String (T.concat vs)
      else
        Object $ HM.fromListWith mergeObject
                   $ attrs
                  ++ values
                  ++ map (second elementToJSON) cs
  where
    attrs = if null as
              then []
              else [("__attributes", Object (attrsToObject as))]
    values = if null vs
               then []
               else [("__values", Array (V.fromList (map String vs)))]

    attrsToObject :: [(T.Text, T.Text)] -> Object
    attrsToObject = HM.fromList . map (second String)

    mergeObject :: Value -> Value -> Value
    mergeObject v (Array arr) = Array (V.snoc arr v)
    mergeObject v1         v2 = Array (V.fromList [v2, v1])

-- | Consume a list of `T.Token' to build an `Element'
tokensToElement :: [T.Token] -> Element
tokensToElement ts = runBuilder (mapM_ tokenToBuilder ts)

-- |Convert list of tagstream-conduit `T.Token` to aeson `Value', combining of `tokensToElement' and `elementToJSON'
tokensToJSON :: [T.Token] -> Value
tokensToJSON = elementToJSON . tokensToElement

newtype JSONParseError = JSONParseError String
  deriving (Typeable, Show)
instance Exception JSONParseError

-- | parse xml to haskell data type by using aeson's `FromJSON'.
parseXML :: (MonadThrow m, FromJSON a) => L.ByteString -> m a
parseXML s = xmlToJSON s >>= convert
  where
    convert v =
        case fromJSON v of
            Error err -> monadThrow (JSONParseError err)
            Success a -> return a

-- | Parse lazy xml `ByteString' to aeson `Value'.
xmlToJSON :: MonadThrow m => L.ByteString -> m Value
xmlToJSON s = liftM tokensToJSON $ C.sourceList (L.toChunks s) $= T.tokenStreamBS $$ C.consume
