module CodeMirror.Component where

import Prelude
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CSS (fromString, width)
import CSS.Display (float, floatLeft)
import CodeMirror.Position (Position, initialPosition)
import CodeMirror.TextMarker (TextMarkerId, TextMarker)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.State.Class (class MonadState, gets, modify)
import Coq.Position (nextSentence)
import Data.Maybe (Maybe(..))
import Data.Options (Options, opt, (:=))
import Data.Traversable (traverse, traverse_)
import Halogen.HTML.CSS (style)
import Halogen.Query (RefLabel(..))
import Ports.CodeMirror as PCM
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
  | HandleChange PCM.CodeMirrorChange (H.SubscribeStatus -> a)
  | HandleKey    a
  | Init         a

data Message
  = Sentence TextMarkerId String

addMarker :: ∀ m. MonadState State m => Position -> Position -> String -> m TextMarker
addMarker from to sentence = do
  nextMarkerId <- gets _.nextMarkerId
  let newMarker = { id    : nextMarkerId
                  , from
                  , to
                  , sentence
                  , stage : ToProcess
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

render :: State -> H.ComponentHTML Query
render { code, cursorPosition, markers, tip } =
  let { line, ch } = cursorPosition in
  HH.div_
    [ HH.button [ HE.onClick $ HE.input_ AddMarker ]
                [ HH.text "+" ]
    , HH.p_ [ HH.text ("Line is: " <> show line)]
    , HH.div
        [ HP.ref (RefLabel "codemirror")
        , style do
            float floatLeft
            width $ fromString "50%"
        ]
        []
    , HH.pre
        [ style do
            float floatLeft
            width $ fromString "50%"
        ]
        [ HH.text code ]
    ]

type DSL = H.ComponentDSL State Query Message

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
                                    , extraKeys   = Just keyMap
                                    , lineNumbers = Just true
                                    , mode        = Just "text/x-ocaml"
                                    , value       = Just code
                                    }
          )
      H.modify (_ { codeMirror = Just cm })
      H.subscribe $ H.eventSource (PCM.onCodeMirrorChange cm) (Just <<< H.request <<< HandleChange)
    pure next

  HandleChange change k -> do
    H.liftEff $ log $ "Change, removed: " <> change.changeObj.removed
    H.gets _.codeMirror >>= traverse_ \ cm -> do
      code <- H.liftEff $ do
        doc <- PCM.getDoc cm
        PCM.getValue doc Nothing
      H.modify (_ { code = code })
    pure $ k H.Listening

  HandleKey next -> do
    H.liftEff $ log "Handle key handler called"
    pure next

  where
    keyMap :: Options PCM.KeyMap
    keyMap =
      opt "Ctrl-Alt-Down" := log "yolo"

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
