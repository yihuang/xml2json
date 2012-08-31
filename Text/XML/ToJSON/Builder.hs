{-# LANGUAGE OverloadedStrings #-}
module Text.XML.ToJSON.Builder where

import Data.Text (Text)
import Control.Monad.Trans.State

type Str = Text

data Element = Element
  { elAttrs       :: [(Str, Str)]
  , elValues      :: [Str]
  , elChildren    :: [(Str, Element)]
  } deriving (Show)

emptyElement :: Element
emptyElement = Element [] [] []

addChild' :: (Str, Element) -> Element -> Element
addChild' item o = o { elChildren = item : elChildren o }

addValue' :: Str -> Element -> Element
addValue' v o = o { elValues = v : elValues o }

addAttr' :: (Str, Str) -> Element -> Element
addAttr' attr o = o { elAttrs = attr : elAttrs o }

addAttrs' :: [(Str, Str)] -> Element -> Element
addAttrs' as o = o { elAttrs = as ++ elAttrs o }

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

beginElement :: Str -> Builder
beginElement name =
    modify ( (name, emptyElement) : )

endElement :: Builder
endElement =
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
