import System.Environment (getArgs)
import Data.Conduit
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Binary as C
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Aeson (encode)

import Text.HTML.TagStream (tokenStream)
import Text.XML.ToJSON (tokensToJSON)

main :: IO ()
main = do
    [name] <- getArgs
    tokens <- runResourceT $ C.sourceFile name $= tokenStream $$ C.consume
    let json = tokensToJSON tokens
    L.putStrLn $ encode json
