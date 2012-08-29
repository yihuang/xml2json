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
    pushElement (decodeUtf8 s)
    addAttrs (map (\(s1,s2) -> (decodeUtf8 s1, decodeUtf8 s2)) as)
    when selfClose popElement
tokenToBuilder (TagClose _) = popElement -- FIXME should match tag name?
tokenToBuilder (Text s) = addValue (decodeUtf8 s)
tokenToBuilder _ = return ()

attrsToObject :: [(Str, Str)] -> Object
attrsToObject = HM.fromList . map (second String)

elementToJSON :: Element -> Value
elementToJSON (Element as vs cs) =
    Object $ HM.fromList $ ("_attributes", Object (attrsToObject as))
                         : ("_values", Array (V.fromList (map String vs)))
                         : map (second elementToJSON) cs

tokensToJSON :: [Token] -> Value
tokensToJSON tokens =
    elementToJSON $ runBuilder (mapM_ tokenToBuilder tokens)
