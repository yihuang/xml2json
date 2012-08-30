{-# LANGUAGE OverloadedStrings #-}
module Text.XML.ToJSON
  ( elementToJSON
  , tokensToJSON
  ) where

import System.IO (stdout)
import Control.Monad (when)
import Control.Arrow (second)

import qualified Data.Text as T
import Data.Conduit
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Binary as C
import Data.Conduit.Blaze (builderToByteString)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Text.HTML.TagStream
import Text.HTML.TagStream.Text
import Text.XML.ToJSON.Builder
import Data.Aeson (Value(..), Object)

tokenToBuilder :: Token -> Builder
tokenToBuilder (TagOpen s as selfClose) = do
    beginElement s
    addAttrs as
    when selfClose endElement
tokenToBuilder (TagClose _) = endElement -- FIXME should match tag name?
tokenToBuilder (Text s) = addValue s
tokenToBuilder _ = return ()

attrsToObject :: [(Str, Str)] -> Object
attrsToObject = HM.fromList . map (second String)

mergeObject :: Value -> Value -> Value
mergeObject (Array arr) v  = Array (V.cons v arr)
mergeObject v1          v2 = Array (V.fromList [v1, v2])

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

tokensToJSON :: [Token] -> Value
tokensToJSON tokens =
    elementToJSON $ runBuilder (mapM_ tokenToBuilder tokens)
