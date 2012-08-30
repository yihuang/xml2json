{-# LANGUAGE OverloadedStrings #-}
import Debug.Trace
import System.Environment (getArgs)
import Control.Applicative
import Data.Maybe (fromMaybe)
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import qualified Data.Text.Lazy as L
import qualified Blaze.ByteString.Builder as B
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8 (char)
import Data.Conduit
import Data.Conduit.Internal (ResumableSource(ResumableSource))
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.Text as C
import qualified Data.Conduit.Attoparsec as C
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Aeson (encode, Value)

import Text.HTML.TagStream
import qualified Text.HTML.TagStream.ByteString as S
import qualified Text.HTML.TagStream.Text as T
import Text.XML.ToJSON (tokensToJSON)

getCodec :: CI.CI ByteString -> Maybe C.Codec
getCodec c =
    case c of
        "utf-8" -> Just C.utf8
        "utf8"  -> Just C.utf8
        "gbk"   -> Just C.iso8859_1
        _       -> Nothing

traceM a = trace (show a) (return ())



xmlToJSON :: (Monad m, MonadThrow m) => Source m ByteString -> m Value
xmlToJSON src = do
    (src', mtoken) <- src $$+ C.sinkParser (char '<' *> S.tag)

    let (mencoding, src'') =
          case mtoken of
            Nothing -> error "no token"
            Just (TagOpen "?xml" as _) -> (lookup "encoding" as, src')
            Just token -> (Nothing, appendResumableSource (yield (B.toByteString (S.showToken id token))) src')

        codec = fromMaybe C.utf8 (CI.mk <$> mencoding >>= getCodec)

    traceM mencoding

    tokens <- src'' $$+- (C.decode codec =$ T.tokenStream =$ C.consume)

    return (tokensToJSON tokens)
  where
    appendResumableSource src (ResumableSource src' close) = ResumableSource (src >> src') close

main :: IO ()
main = do
    [name] <- getArgs
    json <- runResourceT $ xmlToJSON (C.sourceFile name)
    L.putStrLn $ encode json
