module Ports.CodeMirror where

import CodeMirror.CodeMirror (CodeMirrorId)
import Control.Monad.Eff (Eff)
import DOM.HTML.Types (HTMLElement)
import Data.Function.Uncurried
-- import Position (Position)
-- import TextMarker (TextMarker, TextMarkerId)

type EditorConfiguration =
  { autofocus :: Boolean
  , mode      :: String
  , value     :: String
  }

data Editor

foreign import _codeMirror :: forall e. Fn2 HTMLElement EditorConfiguration (Eff (| e) Editor)
codeMirror :: ∀ e. HTMLElement -> EditorConfiguration -> Eff (| e) Editor
codeMirror = runFn2 _codeMirror

foreign import _hasFocus :: forall e. Fn1 Editor (Eff (| e) Boolean)
hasFocus :: ∀ e. Editor -> Eff (| e) Boolean
hasFocus = runFn1 _hasFocus

{-
foreign import markText   :: ( CodeMirrorId, TextMarkerId, Position, Position, TextMarkerOptions ) -> Cmd msg
foreign import setValue   :: ( CodeMirrorId, String ) -> Cmd msg
foreign import getCursor  :: (( CodeMirrorId, Position ) -> msg) -> Sub msg
foreign import getValue   :: (( CodeMirrorId, String ) -> msg) -> Sub msg

foreign import codeMirror :: ( CodeMirrorId, String, String ) -> Cmd msg
foreign import markText   :: ( CodeMirrorId, TextMarkerId, Position, Position, TextMarkerOptions ) -> Cmd msg
foreign import setValue   :: ( CodeMirrorId, String ) -> Cmd msg
foreign import getCursor  :: (( CodeMirrorId, Position ) -> msg) -> Sub msg
foreign import getValue   :: (( CodeMirrorId, String ) -> msg) -> Sub msg

foreign import editImpl :: forall eff. Fn2 String Ace (Eff (ace :: ACE | eff) Editor)

edit :: forall eff. String -> Ace -> Eff (ace :: ACE | eff) Editor
edit el self = runFn2 editImpl el self

foreign import editNodeImpl :: forall eff. Fn2 HTMLElement Ace (Eff (ace :: ACE | eff) Editor)

editNode :: forall eff. HTMLElement -> Ace -> Eff (ace :: ACE | eff) Editor
editNode el self = runFn2 editNodeImpl el self

foreign import createEditSessionForDocumentImpl :: forall eff. Fn3 Document TextMode Ace (Eff (ace :: ACE | eff) EditSession)

createEditSessionForDocument :: forall eff. Document -> TextMode -> Ace -> Eff (ace :: ACE | eff) EditSession
createEditSessionForDocument text mode self = runFn3 createEditSessionForDocumentImpl text mode self

foreign import createEditSessionImpl :: forall eff. Fn3 String TextMode Ace (Eff (ace :: ACE | eff) EditSession)

createEditSession :: forall eff. String -> TextMode -> Ace -> Eff (ace :: ACE | eff) EditSession
createEditSession text mode self = runFn3 createEditSessionImpl text mode self
-}

-- port onChange : (String -> msg) -> Sub msg
