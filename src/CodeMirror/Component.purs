module CodeMirror.Component where

import Prelude
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CodeMirror.Position (Position, initialPosition, nextPosition, unPosition)
import CodeMirror.TextMarker (TextMarkerId, TextMarker)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.State.Class (get, put)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Halogen.Query (RefLabel(..))
import Ports.CodeMirror (CodeMirror, codeMirror)
import Stage (Stage(..))

type State =
  { code           :: String
  , codeMirror     :: Maybe CodeMirror
  , containerId    :: String
  , cursorPosition :: Position
  , markers        :: Map.Map TextMarkerId TextMarker
  , nextMarkerId   :: TextMarkerId
  , tip            :: Position
  }

type Input =
  { code        :: String
  , containerId :: String
  }

initialState :: Input -> State
initialState { code, containerId } =
  { code
  , codeMirror     : Nothing
  , containerId
  , cursorPosition : initialPosition
  , markers        : Map.empty
  , nextMarkerId   : 0
  , tip            : initialPosition
  }

data Query a
  = AddMarker                a
  | Init                     a

data Message
  = AddedMarker TextMarkerId Position Position

addMarker :: Position -> Position -> State -> { marker :: TextMarker, codeMirror :: State }
addMarker from to (cm@{ markers, nextMarkerId }) =
  let newMarker =
        { id: nextMarkerId
        , from
        , to
        , stage : ToProcess
        }
  in
   { marker : newMarker
   , codeMirror : cm { nextMarkerId = nextMarkerId + 1
                     , markers = Map.insert newMarker.id newMarker markers
                     , tip = to
                     }
   }

nextTip :: State -> Maybe Position
nextTip { code, tip } = nextPosition code tip

nextMarker :: State -> Maybe { marker :: TextMarker, codeMirror :: State }
nextMarker (cm@{ code, tip }) = do
  newTip <- nextTip cm
  pure $ addMarker tip newTip cm

render :: State -> H.ComponentHTML Query
render { code, cursorPosition, markers, tip } =
  let { line, ch } = unPosition cursorPosition in
  HH.div_
    [ HH.button [ HE.onClick $ HE.input_ AddMarker ]
                [ HH.text "+" ]
    , HH.button [ HE.onClick $ HE.input_ Init ]
                [ HH.text "init" ]
    , HH.p_ [ HH.text ("Line: " <> show line)]
    , HH.div [ HP.ref (RefLabel "codemirror") ] []
    ]

eval :: forall e m. MonadEff (console :: CONSOLE | e) m => Query ~> H.ComponentDSL State Query Message m
eval = case _ of

  AddMarker next -> do
    cm <- get
    case nextMarker cm of
      Nothing -> pure next
      Just { marker, codeMirror } -> do
        put codeMirror
        H.raise $ AddedMarker marker.id marker.from marker.to
        pure next

  Init next -> do
    { code } <- get
    --liftEff $ log "Initializing CodeMirror"
    _ <- H.getHTMLElementRef (H.RefLabel "codemirror") >>= traverse \ element -> do
      --liftEff $ log "Found RefLabel codemirror"
      codeMirror <- liftEff $ do
        codeMirror element { autofocus   : true
                           , lineNumbers : true
                           , mode        : "text/x-ocaml"
                           , value       : code
                           }
      H.modify (_ { codeMirror = Just codeMirror })
      -- H.subscribe $ H.eventSource_ (Session.onChange session) (H.request HandleChange)
    pure next

ui :: forall e m. MonadEff (console :: CONSOLE | e) m => H.Component HH.HTML Query Input Message m
ui = H.lifecycleComponent
     { initialState : initialState
     , render
     , eval
     , receiver     : const Nothing
     , initializer  : Just $ H.action Init
     , finalizer    : Nothing
     }
