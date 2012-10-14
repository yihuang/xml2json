{-# LANGUAGE OverloadedStrings #-}
module Text.XML.ToJSON.Builder
  ( -- * Element type and operations
    Element(..)
  , emptyElement
  , addChild'
  , addValue'
  , addAttr'
  , addAttrs'
    -- * Stack type and operations
  , Stack
  , popStack
  , closeStack
    -- * Builder type and operations
  , Builder
  , runBuilder
  , beginElement
  , endElement
  , modifyTopElement
  , addChild
  , addValue
  , addAttr
  , addAttrs
  ) where

import Data.Text (Text)
import Control.Monad.Trans.State

-- | represent a XML element.
data Element = Element
  { elAttrs       :: [(Text, Text)]     -- ^ tag attributes.
  , elValues      :: [Text]             -- ^ text values.
  , elChildren    :: [(Text, Element)]  -- ^ child elements.
  } deriving (Show)

emptyElement :: Element
emptyElement = Element [] [] []

reverseChildren :: Element -> Element
reverseChildren (Element as vs cs) = Element as vs (reverse cs)

-- | add a child element to an element, leave children in reverse order, reverse it in `popStack'.
addChild' :: (Text, Element) -> Element -> Element
addChild' item o = o { elChildren = item : elChildren o }

-- | add a text value to an element
addValue' :: Text -> Element -> Element
addValue' v o = o { elValues = v : elValues o }

-- | add an attribute to an element
addAttr' :: (Text, Text) -> Element -> Element
addAttr' attr o = o { elAttrs = attr : elAttrs o }

-- | add multiple attributes to an element
addAttrs' :: [(Text, Text)] -> Element -> Element
addAttrs' as o = o { elAttrs = as ++ elAttrs o }

-- | xml element stack with recent opened element at the top.
type Stack = [(Text, Element)]

-- | close current tag.
popStack :: Stack -> Stack
popStack ((k,v) : (name,elm) : tl) = (name, addChild' (k,reverseChildren v) elm) : tl
popStack _ = error "popStack: can't pop root elmect."

-- | close all unclosed tags and return the root element.
closeStack :: Stack -> Element
closeStack []          = error "closeStack: empty stack."
closeStack [(_, elm)]  = reverseChildren elm
closeStack st          = closeStack (popStack st)

-- | `Builder' is a `State' monad to transform a `Stack'.
type Builder = State Stack ()

-- | exec the state monad and close the result stack.
runBuilder :: Builder -> Element
runBuilder b = closeStack $ execState b [("", emptyElement)]

-- | open element
beginElement :: Text -> Builder
beginElement name =
    modify ( (name, emptyElement) : )

-- | close element
endElement :: Builder
endElement =
    modify popStack

-- | util to modify top element.
modifyTopElement :: (Element -> Element) -> Builder
modifyTopElement f =
    modify $ \st ->
        case st of
            ((k, v) : tl) -> (k, f v) : tl
            _       -> fail "modifyTopElement: impossible: empty stack."

-- | add value to top element.
addValue :: Text -> Builder
addValue = modifyTopElement . addValue'

-- | add attribute to top element.
addAttr :: (Text, Text) -> Builder
addAttr = modifyTopElement . addAttr'

-- | add multiple attributes to top element.
addAttrs :: [(Text, Text)] -> Builder
addAttrs = modifyTopElement . addAttrs'

-- | add child element to top element.
addChild :: (Text, Element) -> Builder
addChild = modifyTopElement . addChild'
