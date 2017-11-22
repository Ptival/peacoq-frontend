module Chick.Component where

import Prelude
import CSS as CSS
import CSS.Display as CSSD
import CodeMirror.Component as CM
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import SerAPI.Component as SAPI
import CodeMirror.TextMarker (TextMarkerId)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Console (log)
import Data.Either.Nested (Either2)
import Data.Foldable (fold)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Lens (lens, over, view)
import Data.Lens.Types (Lens')
import Data.Maybe (Maybe(..))
--import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Halogen.Component.ChildPath (ChildPath, cp1, cp2)
import Halogen.HTML.CSS (style)
import Network.HTTP.Affjax (AJAX)

-- this seems a little dumb, but looks like we have to...
data Query a
  = CodeMirrorUpdated String String a

type ChildQuery = Coproduct2 CM.Query SAPI.Query

type State =
  { codeBefore :: String
  , codeAfter  :: String
  }

_codeBefore :: Lens' State String
_codeBefore = lens _.codeBefore (_ { codeBefore = _ })

_codeAfter :: Lens' State String
_codeAfter = lens _.codeAfter (_ { codeAfter = _ })

type Slot = Either2 Unit Unit

cmSlot :: ChildPath CM.Query ChildQuery Unit Slot
cmSlot = cp1

sapiSlot :: ChildPath SAPI.Query ChildQuery Unit Slot
sapiSlot = cp2

type Input = Unit

type Message = Void

handleCodeMirror :: CM.Message -> Maybe (Query Unit)
handleCodeMirror = case _ of
  CM.Updated bef aft -> Just $ H.action $ CodeMirrorUpdated bef aft

type ChickEffects e =
  ( ajax    :: AJAX
  , avar    :: AVAR
  , console :: CONSOLE
  | e)

type Render m = H.ParentHTML Query ChildQuery Slot m

render :: ∀ m e. MonadAff (ChickEffects e) m => State -> Render m
render state =
  HH.div [ style $ do
              CSS.height    $ CSS.fromString "100%"
              CSS.minHeight $ CSS.fromString "100%"
         ]
  $
  [ HH.slot'
    cmSlot
    unit
    CM.codeMirrorComponent { code : initialCode }
    handleCodeMirror
  ]

eval :: ∀ e m. MonadAff (ChickEffects e) m => Query ~> H.ParentDSL State Query ChildQuery Slot Message m
eval = case _ of

  CodeMirrorUpdated bef aft next -> do
    H.liftEff $ do
      log bef
      log aft
    pure next

peaCoqComponent :: ∀ e m. MonadAff (ChickEffects e) m => H.Component HH.HTML Query Input Message m
peaCoqComponent =
  H.parentComponent
    { initialState : const { codeBefore : "", codeAfter : "" }
    , render
    , eval
    , receiver     : const Nothing
    }

initialCode :: String
initialCode = """
Definition f : (A → B) → A → B := λ f a, f a.
"""
