{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import Data.Conduit
import qualified Data.Conduit.Binary as C
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Aeson (encode)
import Text.XML.ToJSON (xmlToJSON)

main :: IO ()
main = do
    [name] <- getArgs
    json <- runResourceT $ xmlToJSON (C.sourceFile name)
    L.putStrLn $ encode json
