Translate xml to json, and due to using of `tagstream-conduit <https://github.com/yihuang/tagstream-conduit>`_, it can parse malformed xml.

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
