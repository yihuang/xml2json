{-# LANGUAGE OverloadedStrings #-}
module Text.XML.ToJSON
  ( elementToJSON
  , tokensToJSON
  , xmlToJSON
  ) where

import Control.Monad (when)
import Control.Arrow (second)
import Control.Applicative ( (<$>), (*>), (<|>) )

import Data.Maybe (fromMaybe)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Blaze.ByteString.Builder as B
import qualified Data.Attoparsec as A
import Data.Attoparsec.ByteString.Char8 (char, skipSpace)
import Data.Conduit
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
import Data.Aeson (Value(..), Object)

tokenToBuilder :: T.Token -> Builder
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

tokensToJSON :: [T.Token] -> Value
tokensToJSON tokens =
    elementToJSON $ runBuilder (mapM_ tokenToBuilder tokens)

xmlToJSON :: (Functor m, Monad m, MonadThrow m) => Source m ByteString -> m Value
xmlToJSON src = xmlToJSONResumable (ResumableSource src (return ()))

skipBOM :: A.Parser ()
skipBOM =
    ( A.string "\xff\xfe"
    <|> A.string "\xef\xbb\xbf"
    ) *> return ()

xmlToJSONResumable :: (Functor m, Monad m, MonadThrow m) => ResumableSource m ByteString -> m Value
xmlToJSONResumable src = do
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

    tokensToJSON <$> (src'' $$+- (C.decode codec =$ T.tokenStream =$ C.consume))

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

