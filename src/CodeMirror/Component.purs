module CodeMirror.Component where

import Prelude
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CSS (fromString, width)
import CSS.Display (float, floatLeft)
import CodeMirror.Position (Position, initialPosition, unPosition)
import CodeMirror.TextMarker (TextMarkerId, TextMarker)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.State.Class (class MonadState, gets, modify)
import Coq.Position (nextSentence)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Halogen.HTML.CSS (style)
import Halogen.Query (RefLabel(..))
import Ports.CodeMirror (CodeMirror, codeMirror, getDoc, getValue, markText, onCodeMirrorChange)
import Stage (Stage(..))

type CodeMirrorEffects e =
  ( avar    :: AVAR
  , console :: CONSOLE
  | e)

type State =
  { code           :: String
  , codeMirror     :: Maybe CodeMirror
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
  | HandleChange (H.SubscribeStatus -> a)
  | Init         a

data Message = Unit

addMarker :: forall m. MonadState State m => Position -> Position -> m TextMarker
addMarker from to = do
  nextMarkerId <- gets _.nextMarkerId
  let newMarker = { id    : nextMarkerId
                  , from
                  , to
                  , stage : ToProcess
                  }
  modify (\ s -> s { nextMarkerId = nextMarkerId + 1
                   , markers      = Map.insert newMarker.id newMarker s.markers
                   })
  pure newMarker

nextTip :: State -> Maybe Position
nextTip { code, tip } = nextSentence code tip <#> _.position

nextMarker :: forall m. MonadState State m => m (Maybe TextMarker)
nextMarker = do
  gets nextTip >>= traverse \ newTip -> do
    tip <- gets _.tip
    newMarker <- addMarker tip newTip
    modify (\ s -> s { tip = newMarker.to })
    pure newMarker

render :: State -> H.ComponentHTML Query
render { code, cursorPosition, markers, tip } =
  let { line, ch } = unPosition cursorPosition in
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

eval :: forall e m. MonadAff (CodeMirrorEffects e) m => Query ~> H.ComponentDSL State Query Message m
eval = case _ of

  AddMarker next -> do
    nextMarker >>= traverse_ \ marker -> do
      gets _.codeMirror >>= traverse_ \ codeMirror -> H.liftEff $ do
        doc <- getDoc codeMirror
        -- log "About to mark text"
        let textMarkerOptions =
              Just { className : "bgGreen"
                   }
        markText doc marker.from marker.to textMarkerOptions
    pure next

  Init next -> do
    { code } <- H.get
    --liftEff $ log "Initializing CodeMirror"
    H.getHTMLElementRef (H.RefLabel "codemirror") >>= traverse_ \ element -> do
      --liftEff $ log "Found RefLabel codemirror"
      cm <- H.liftEff do
        codeMirror element { autofocus   : true
                           , lineNumbers : true
                           , mode        : "text/x-ocaml"
                           , value       : code
                           }
      H.modify (_ { codeMirror = Just cm })
      H.subscribe $ H.eventSource_ (onCodeMirrorChange cm) (H.request HandleChange)
    pure next

  HandleChange k -> do
    H.gets _.codeMirror >>= traverse_ \ cm -> do
      code <- H.liftEff $ do
        doc <- getDoc cm
        getValue doc Nothing
      H.modify (_ { code = code })
    pure $ k H.Listening

codeMirrorComponent :: forall e m. MonadAff (CodeMirrorEffects e) m => H.Component HH.HTML Query Input Message m
codeMirrorComponent = H.lifecycleComponent
     { initialState : initialState
     , render
     , eval
     , receiver     : const Nothing
     , initializer  : Just $ H.action Init
     , finalizer    : Nothing
     }
