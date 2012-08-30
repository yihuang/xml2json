{-# LANGUAGE OverloadedStrings #-}
module Text.XML.ToJSON
  ( elementToJSON
  , tokensToJSON
  ) where

import System.IO (stdout)
import Control.Monad (when)
import Control.Arrow (second)

import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Conduit
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Binary as C
import Data.Conduit.Blaze (builderToByteString)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Text.HTML.TagStream (tokenStream, showToken, Token, Token'(..))
import Text.XML.ToJSON.Builder
import Data.Aeson (Value(..), Object)

tokenToBuilder :: Token -> Builder
tokenToBuilder (TagOpen s as selfClose) = do
    beginElement (decodeUtf8 s)
    addAttrs (map (\(s1,s2) -> (decodeUtf8 s1, decodeUtf8 s2)) as)
    when selfClose endElement
tokenToBuilder (TagClose _) = endElement -- FIXME should match tag name?
tokenToBuilder (Text s) = addValue (decodeUtf8 s)
tokenToBuilder _ = return ()

attrsToObject :: [(Str, Str)] -> Object
attrsToObject = HM.fromList . map (second String)

mergeObject :: Value -> Value -> Value
mergeObject (Array arr) v  = Array (V.cons v arr)
mergeObject v1          v2 = Array (V.fromList [v1, v2])

elementToJSON :: Element -> Value
elementToJSON (Element as vs cs) =
    Object $ HM.fromListWith mergeObject
               $ ("__attributes", Object (attrsToObject as))
               : ("__values", Array (V.fromList (map String vs)))
               : map (second elementToJSON) cs

tokensToJSON :: [Token] -> Value
tokensToJSON tokens =
    elementToJSON $ runBuilder (mapM_ tokenToBuilder tokens)
