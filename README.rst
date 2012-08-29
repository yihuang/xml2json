Translate xml to json, and thanks to tagstream-conduit, it can parse malformed xml.

Example
=======

XML ::

  <test "k1"="v1" "k2"="v2">
      aaa
      <p>bbb</p>
      ccc
  </test>

would be translated to JSON ::

  { "_values":["\n"]
  , "_attributes":{}
  , "test":{ "_values":["\n    ccc\n","aaa\n    "]
           , "_attributes":{"k2":"v2","k1":"v1"}
           , "p":{ "_values":["bbb"]
                 , "_attributes":{}
                 }
           }
  }
