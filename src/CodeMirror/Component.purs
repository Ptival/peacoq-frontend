module CodeMirror.Component where

import Prelude
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CodeMirror.CodeMirror (CodeMirrorId, CodeMirror)
import CodeMirror.Position (Position, initialPosition, nextPosition, unPosition)
import CodeMirror.TextMarker (TextMarkerId, TextMarker)
import Control.Monad.State.Class (get, put)
import Data.Maybe (Maybe(..))
import Stage (Stage(..))

type State = CodeMirror

type Input =
  { code :: String
  , id   :: CodeMirrorId
  }

initialState :: Input -> State
initialState { code, id } =
  { code           : code
  , cursorPosition : initialPosition
  , id             : id
  , markers        : Map.empty
  , nextMarkerId   : 0
  , tip            : initialPosition
  }

data Query a
  = AddMarker a

data Message
  = AddedMarker TextMarkerId Position Position

addMarker :: Position -> Position -> CodeMirror -> { marker :: TextMarker, codeMirror :: CodeMirror }
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

nextTip :: CodeMirror -> Maybe Position
nextTip { code, tip } = nextPosition code tip

nextMarker :: CodeMirror -> Maybe { marker :: TextMarker, codeMirror :: CodeMirror }
nextMarker (cm@{ code, tip }) = do
  newTip <- nextTip cm
  pure $ addMarker tip newTip cm

ui :: forall m. H.Component HH.HTML Query Input Message m
ui = H.component
     { initialState : initialState
     , render
     , eval
     , receiver : const Nothing
     }
  where
    render { code, cursorPosition, markers, tip } =
      let { line, ch } = unPosition cursorPosition in
      HH.div_ 
        [ HH.button [ HE.onClick $ HE.input_ AddMarker ] 
                    [ HH.text "+" ]
        , HH.p_ [ HH.text ("Line: " <> show line)]
        , HH.div [ HP.id_ "codemirror" ] []
        ]
    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      AddMarker next -> do
        cm <- get
        case nextMarker cm of
          Nothing -> pure next
          Just { marker, codeMirror } -> do
            put codeMirror
            H.raise $ AddedMarker marker.id marker.from marker.to
            pure next
