{-# LANGUAGE OverloadedStrings, TupleSections #-}
import System.Environment (getArgs)
import System.IO (stdin, stdout, openFile, IOMode(..), hFlush)
import System.Exit (exitSuccess)
import Control.Applicative
import Data.Conduit
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.List as C
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Aeson (encode)
import Text.XML.PList (tokensToJSON)
import Text.HTML.TagStream.Text (tokenStreamBS)

main :: IO ()
main = do
    args <- getArgs
    (hInput, hOutput) <- case args of
        []              -> return (stdin, stdout)
        [input]         -> (,stdout) <$> openFile input ReadMode
        [input, output] -> (,) <$> openFile input ReadMode
                               <*> openFile output WriteMode
        _ -> putStrLn "plist2json [input] [output]" >> exitSuccess

    tokens <- runResourceT $ C.sourceHandle hInput $= tokenStreamBS $$ C.consume
    L.hPutStrLn hOutput $ encode $ tokensToJSON tokens
    hFlush hOutput
