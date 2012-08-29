{-# LANGUAGE OverloadedStrings #-}
module Text.XML.ToJSON.Builder where

import Data.Text (Text)
import Control.Monad.Trans.State

type Str = Text

data Element = Element
  { attrs       :: [(Str, Str)]
  , values      :: [Str]
  , children    :: [(Str, Element)]
  } deriving (Show)

emptyElement = Element [] [] []

addChild' :: (Str, Element) -> Element -> Element
addChild' item o = o { children = item : children o }

addValue' :: Str -> Element -> Element
addValue' v o = o { values = v : values o }

addAttr' :: (Str, Str) -> Element -> Element
addAttr' attr o = o { attrs = attr : attrs o }

addAttrs' :: [(Str, Str)] -> Element -> Element
addAttrs' as o = o { attrs = as ++ attrs o }

type Stack = [(Str, Element)]
type Builder = State Stack ()

runBuilder :: Builder -> Element
runBuilder b = finishStack $ execState b [("", emptyElement)]

popStack :: Stack -> Stack
popStack ((k,v) : (name,elm) : tl) = (name, addChild' (k,v) elm) : tl
popStack _ = error "popStack: can't pop root elmect."

finishStack :: Stack -> Element
finishStack []          = error "finishStack: empty stack."
finishStack [(_, elm)]  = elm
finishStack st          = finishStack (popStack st)

pushElement :: Str -> Builder
pushElement name =
    modify ( (name, emptyElement) : )

popElement :: Builder
popElement =
    modify popStack

modifyTopElement :: (Element -> Element) -> Builder
modifyTopElement f =
    modify $ \st ->
        case st of
            ((k, v) : tl) -> (k, f v) : tl
            _       -> fail "modifyTopElement: impossible: empty stack."

addValue :: Str -> Builder
addValue = modifyTopElement . addValue'

addAttr :: (Str, Str) -> Builder
addAttr = modifyTopElement . addAttr'

addAttrs :: [(Str, Str)] -> Builder
addAttrs = modifyTopElement . addAttrs'

addChild :: (Str, Element) -> Builder
addChild = modifyTopElement . addChild'
