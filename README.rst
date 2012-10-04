Translate xml to json, and due to using of `tagstream-conduit <https://github.com/yihuang/tagstream-conduit>`_, it can parse malformed xml.

And when combined with aeson's parsing facility, you can use it to parse xml to your haskell data type.

Examples
========

Simple example
--------------

XML ::

  <test "k1"="v1" "k2"="v2">
      aaa
      <p>bbb</p>
      ccc
  </test>

JSON ::

  {"test":{"p":"bbb"
          ,"__attributes":{"k2":"v2","k1":"v1"}
          ,"__values":["\n    ccc\n","aaa\n    "]
          }
  ,"__values":["\n"]
  }

Siblings with same name got merged.
-----------------------------------

XML ::

  <books>
      <book>
          <name>foo</name>
          <author>Jim</author>
      </book>
      <book>
          <name>bar</name>
          <author>Jake</author>
      </book>
  </books>

JSON ::

  {"__values":["\n"]
  ,"books":{"book":[{"author":"Jim"
                    ,"name":"foo"
                    ,"__values":["\n    ","\n        "]
                    }
                   ,{"author":"Jake"
                    ,"name":"bar"
                    ,"__values":["\n    ","\n        "]
                    }
                   ]
           ,"__values":["\n","\n    "]
           }
  }

Parse xml to haskell data type
==============================

(You can find real example code in tests/Test.hs)

Say you want to parse following xml ::

  <users>
    <count>100</count>
    <user>
      <name>foo</name>
      <addr>foo addr</addr>
    </user>
    <user>
      <name>bar</name>
      <addr>bar addr</addr>
    </user>
  </users>

And You have follow haskell data type ::

    data User = User
      { name :: Text
      , addr :: Text
      } deriving (Eq)

    data UserList = UserList
      { userList :: [User]
      , userCount :: Int
      } deriving (Eq)

You want to parse xml into this data type, you can define `FromJSON` instance for them like this ::

    instance FromJSON User where
        parseJSON (Object o) =
            User <$> o .: "name"
                 <*> o .: "addr"
        parseJSON o = typeMismatch "User" o

    instance FromJSON UserList where
        parseJSON (Object o) = do
            root <- o .: "users"
            UserList <$> root .: "user"
                     <*> (fmap read (root .: "count"))
          where
            parseUserList (Array a) =
                mapM parseJSON (V.toList a)
            parseUserList o = typeMismatch "UserList.userList" o
        parseJSON o = typeMismatch "UserList" o

Then you can use `parseXML` provided by this library to parse the xml bytestring to your data type: ::

    a <- parseXML xml_string
    assert $ a == UserList [User "foo" "foo addr", User "bar" "bar addr"] 100
