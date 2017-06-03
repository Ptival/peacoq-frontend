module CodeMirror.Component where

import Prelude
import CSS as CSS
import CSS.Display as CSSD
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ports.CodeMirror as PCM
import CodeMirror.Position (Position, initialPosition)
import CodeMirror.TextMarker (TextMarkerId, TextMarker)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.State.Class (class MonadState, gets, modify)
import Coq.Position (nextSentence)
import Data.Array (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_, traverse, traverse_)
import Halogen.HTML.CSS (style)
import Halogen.Query (RefLabel(..))
import Stage (Stage(..))

type CodeMirrorEffects e =
  ( avar    :: AVAR
  , console :: CONSOLE
  | e)

type State =
  { code           :: String
  , codeMirror     :: Maybe PCM.CodeMirror
  , cursorPosition :: Position
  , markers        :: Map.Map TextMarkerId TextMarker
  , nextMarkerId   :: TextMarkerId
  , tip            :: Position
  }

type Input =
  { code        :: String
  }

initialState :: Input -> State
initialState { code } =
  { code
  , codeMirror     : Nothing
  , cursorPosition : initialPosition
  , markers        : Map.empty
  , nextMarkerId   : 0
  , tip            : initialPosition
  }

data Query a
  = AddMarker    a
  | Init         a
  | HandleChange PCM.CodeMirrorChange (H.SubscribeStatus -> a)
  | HandleKey    KeyCombination       (H.SubscribeStatus -> a)

data Message
  = Sentence TextMarkerId String

addMarker :: ∀ m. MonadState State m => Position -> Position -> String -> m TextMarker
addMarker from to sentence = do
  nextMarkerId <- gets _.nextMarkerId
  let newMarker = { id       : nextMarkerId
                  , from
                  , to
                  , sentence
                  , stage    : ToProcess
                  }
  modify (\ s -> s { nextMarkerId = nextMarkerId + 1
                   , markers      = Map.insert newMarker.id newMarker s.markers
                   })
  pure newMarker

nextTip :: State -> Maybe { position :: Position, sentence :: String }
nextTip { code, tip } = nextSentence code tip

nextMarker :: ∀ m. MonadState State m => m (Maybe TextMarker)
nextMarker = do
  gets nextTip >>= traverse \ { position : newTip, sentence } -> do
    tip <- gets _.tip
    newMarker <- addMarker tip newTip sentence
    modify (\ s -> s { tip = newMarker.to })
    pure newMarker

renderMarker :: TextMarker -> H.ComponentHTML Query
renderMarker { id, stage } =
  HH.div
  [ HP.title $ show id
  , style $ do
      CSS.width  $ CSS.fromString "100%"
      CSS.height $ CSS.fromString "20px"
      CSS.backgroundColor $ case stage of
        ToProcess  -> CSS.dodgerblue
        Processing -> CSS.darkorange
        Processed  -> CSS.forestgreen
  ]
  [ HH.text (show id) ]

render :: State -> H.ComponentHTML Query
render { code, cursorPosition, markers, tip } =
  HH.div_
    [ HH.div
      [ style $ do
           CSS.display CSS.flex
      ]
      (fromFoldable $ renderMarker <$> Map.values markers)
    , HH.button [ HE.onClick $ HE.input_ AddMarker ]
                [ HH.text "+" ]
    , HH.p_ [ HH.text ("Line is: " <> show cursorPosition.line)]
    , HH.div
        [ HP.ref (RefLabel "codemirror")
        , style do
            CSSD.float CSSD.floatLeft
            CSS.width $ CSS.fromString "50%"
        ]
        []
    , HH.pre
        [ style do
            CSSD.float CSSD.floatLeft
            CSS.width $ CSS.fromString "50%"
        ]
        [ HH.text code ]
    ]

type DSL = H.ComponentDSL State Query Message

data KeyCombination
  = CtrlAltDown
  | CtrlAltUp

type KeyBinding = { key :: KeyCombination, keyS :: String }

bindKey :: KeyCombination -> String -> KeyBinding
bindKey key keyS = { key, keyS }

keyBindings :: Array KeyBinding
keyBindings =
  [ bindKey CtrlAltDown "Ctrl-Alt-Down"
  , bindKey CtrlAltUp   "Ctrl-Alt-Up"
  ]

eval :: ∀ e m. MonadAff (CodeMirrorEffects e) m => Query ~> DSL m
eval = case _ of

  AddMarker next -> do
    nextMarker >>= traverse_ \ marker -> do
      gets _.codeMirror >>= traverse_ \ cm -> H.liftEff $ do
        doc <- PCM.getDoc cm
        let textMarkerOptions =
              Just { className : "bgGreen"
                   }
        PCM.markText doc marker.from marker.to textMarkerOptions
      H.raise $ Sentence marker.id marker.sentence
    pure next

  Init next -> do
    { code } <- H.get
    H.getHTMLElementRef (H.RefLabel "codemirror") >>= traverse_ \ element -> do
      cm <- H.liftEff do
        PCM.codeMirror element
          (PCM.defaultConfiguration { autofocus   = Just true
                                    , lineNumbers = Just true
                                    , mode        = Just "text/x-ocaml"
                                    , value       = Just code
                                    }
          )
      H.modify (_ { codeMirror = Just cm })
      H.subscribe $ H.eventSource (PCM.onCodeMirrorChange cm)              (Just <<< H.request <<< HandleChange)
      for_ keyBindings \ { key, keyS } -> do
        H.subscribe $ H.eventSource (PCM.addKeyMap cm keyS false) (Just <<< H.request <<< const (HandleKey key))
    pure next

  HandleChange change k -> do
    H.liftEff $ log $ "Change, removed: " <> change.changeObj.removed
    H.gets _.codeMirror >>= traverse_ \ cm -> do
      code <- H.liftEff $ do
        doc <- PCM.getDoc cm
        PCM.getValue doc Nothing
      H.modify (_ { code = code })
    pure $ k H.Listening

  HandleKey u k -> do
    case u of
      CtrlAltDown -> do
        H.liftEff $ log "On Ctrl Alt Down"
      CtrlAltUp -> do
        H.liftEff $ log "On Ctrl Alt Up"
    pure $ k H.Listening

codeMirrorComponent ::
  ∀ e m. MonadAff (CodeMirrorEffects e) m => H.Component HH.HTML Query Input Message m
codeMirrorComponent = H.lifecycleComponent
  { initialState : initialState
  , render
  , eval
  , receiver     : const Nothing
  , initializer  : Just $ H.action Init
  , finalizer    : Nothing
  }
