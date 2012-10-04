{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Text.XML.ToJSON
  (

{-| This library provide a way to convert xml to json.
    
    Further more, by combining with aeson's parsing facility, it provide a way to parse xml to haskell data type.
 -}
    parseXML
  , xmlToJSON
  , JSONParseError
    -- * streamlined api
  , bsSourceToJSON
  , bsRSourceToJSON
    -- * utils
  , tokenToBuilder
  , elementToJSON
  , tokensToJSON
  ) where

import Control.Monad (when, liftM)
import Control.Arrow (second)
import Control.Exception (Exception)
import Control.Applicative ( (*>), (<|>) )

import Data.Typeable (Typeable)
import Data.Maybe (fromMaybe)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import Data.ByteString (ByteString)
import qualified Blaze.ByteString.Builder as B
import qualified Data.Attoparsec as A
import Data.Attoparsec.ByteString.Char8 (char, skipSpace)
import Data.Conduit (Source, yield, (=$), ($$++), ($$+-), MonadThrow(monadThrow))
import Data.Conduit.Internal (ResumableSource(ResumableSource))
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit.Text as C
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Text.HTML.TagStream
import qualified Text.HTML.TagStream.Text as T
import qualified Text.HTML.TagStream.ByteString as S
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
    mergeObject (Array arr) v  = Array (V.cons v arr)
    mergeObject v1          v2 = Array (V.fromList [v1, v2])

-- |Convert list of tagstream-conduit `Token` to aeson `Value'
tokensToJSON :: [T.Token] -> Value
tokensToJSON tokens =
    elementToJSON $ runBuilder (mapM_ tokenToBuilder tokens)

-- | Consume a source and convert the content to aeson `Value', it try to inspect xml encoding from first tag.
--
-- e.g. @bsSourceToJSON (C.sourceFile path_to_xml_file)@
bsSourceToJSON :: MonadThrow m => Source m ByteString -> m Value
bsSourceToJSON src = bsRSourceToJSON (ResumableSource src (return ()))

-- | Consume a source and convert the content to aeson `Value', it try to inspect xml encoding from first tag.
--
-- e.g. @xmlStreamToJSONResumable (requestBody req)@
bsRSourceToJSON :: MonadThrow m => ResumableSource m ByteString -> m Value
bsRSourceToJSON src = do
    -- try to peek the first tag to find the xml encoding.
    (src', token) <- src $$++ C.sinkParser (skipBOM *> skipSpace *> char '<' *> S.tag)

    let (mencoding, src'') =
          case token of
            (TagOpen "?xml" as _) ->
                (lookup "encoding" as, src')
            _ ->
                ( Nothing
                , prependRSrc
                    (yield (B.toByteString (S.showToken id token)))
                    src'
                )

        codec = fromMaybe C.utf8 (mencoding >>= getCodec . CI.mk)

    liftM tokensToJSON (src'' $$+- (C.decode codec =$ T.tokenStream =$ C.consume))
  where
    skipBOM :: A.Parser ()
    skipBOM =
        ( A.string "\xff\xfe"
        <|> A.string "\xef\xbb\xbf"
        ) *> return ()

    prependRSrc :: Monad m
                => Source m a
                -> ResumableSource m a
                -> ResumableSource m a
    prependRSrc src (ResumableSource src' close) = ResumableSource (src >> src') close

    getCodec :: CI.CI ByteString -> Maybe C.Codec
    getCodec c =
        case c of
            "utf-8"   -> Just C.utf8
            "utf8"    -> Just C.utf8
            "iso8859" -> Just C.iso8859_1
            _         -> Nothing

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

-- | convert lazy xml `ByteString' to aeson `Value'.
xmlToJSON :: MonadThrow m => L.ByteString -> m Value
xmlToJSON s = bsSourceToJSON (C.sourceList (L.toChunks s))
