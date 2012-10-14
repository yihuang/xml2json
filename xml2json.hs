{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import Data.Conduit
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.List as C
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Aeson (encode)
import Text.XML.ToJSON (tokensToJSON)
import Text.HTML.TagStream.Text (tokenStreamBS)

main :: IO ()
main = do
    [name] <- getArgs
    tokens <- runResourceT $ C.sourceFile name $= tokenStreamBS $$ C.consume
    L.putStrLn $ encode $ tokensToJSON tokens
