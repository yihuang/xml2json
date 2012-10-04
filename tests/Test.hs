{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (liftM)
import Control.Monad.Trans.Resource (runExceptionT)
import Control.Exception (throw)
import Data.Functor.Identity (runIdentity)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Encoding as T
import Data.Aeson (encode)
import Text.XML.ToJSON (xmlToJSON)
import Test.Hspec

toLazy s = L.fromChunks [s]

cases :: [(String, L.ByteString, L.ByteString)]
cases =
  [ ( "basic"
    , toLazy $ T.encodeUtf8 $
      "<user>\
         \<name>foo</name>\
         \<addr>bar road</addr>\
      \</user>"
    , toLazy $ T.encodeUtf8 $
      "{\"user\":{\"name\":\"foo\",\"addr\":\"bar road\"}}"
    )
  , ( "unicode"
    , toLazy $ T.encodeUtf8 $
      "<?xml encoding=\"utf-8\"?>\n\
      \<user>\
         \<name>测试</name>\
         \<addr>bar road</addr>\
      \</user>"
    , toLazy $ T.encodeUtf8 $
      "{\"user\":{\"name\":\"测试\",\"addr\":\"bar road\"}}"
    )
  , ( "array"
    , toLazy $ T.encodeUtf8 $
      "<users>\
        \<user>\
         \<name>foo</name>\
         \<addr>foo road</addr>\
        \</user>\
        \<user>\
         \<name>bar</name>\
         \<addr>bar road</addr>\
        \</user>\
        \<user>\
         \<name>test</name>\
         \<addr>test road</addr>\
        \</user>\
      \</users>"
    , toLazy $ T.encodeUtf8 $
      "{\"users\":\
         \{\"user\":\
            \[{\"name\":\"foo\",\"addr\":\"foo road\"}\
            \,{\"name\":\"bar\",\"addr\":\"bar road\"}\
            \,{\"name\":\"test\",\"addr\":\"test road\"}\
            \]\
         \}\
       \}"
    )
  ]

one (desc, xml, json) =
    it desc $
        let m = liftM encode (xmlToJSON xml)
        in  either throw
                   (==json)
                   (runIdentity (runExceptionT m))

main :: IO ()
main = hspec $ do
    describe "basic cases" $ mapM_ one cases
