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
import Ports.CodeMirror.Configuration as CFG
import Ports.CodeMirror.TextMarkerOptions as TMO
import CodeMirror.Position (Position, initialPosition)
import CodeMirror.TextMarker (TextMarker, TextMarkerId)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.State.Class (class MonadState, gets, modify)
import Coq.Position (nextSentence)
import Data.Array (fromFoldable)
import Data.Lens (lens, over, view)
import Data.Lens.Types (Lens')
import Data.Lens.Zoom (zoom)
import Data.List (fold)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_, traverse, traverse_)
import Halogen.HTML.CSS (style)
import Halogen.Query (RefLabel(..))
import SerAPI.Answer (AnswerKind(..))
import Stage (Stage(..), stageColor)

type TextMarkerWithReference =
  { marker    :: TextMarker
  , reference :: PCM.TextMarker
  }

_marker :: Lens' TextMarkerWithReference TextMarker
_marker = lens _.marker (_ { marker = _ })

_reference :: Lens' TextMarkerWithReference PCM.TextMarker
_reference = lens _.reference (_ { reference = _ })

type State =
  { code           :: String
  , codeMirror     :: Maybe PCM.CodeMirror
  , cursorPosition :: Position
  , markers        :: Map.Map TextMarkerId TextMarkerWithReference
  , nextMarkerId   :: TextMarkerId
  , tip            :: Position
  }

_markers :: Lens' State (Map.Map TextMarkerId TextMarkerWithReference)
_markers = lens _.markers (_ { markers = _ })

type Input =
  { code :: String
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
  = AnswerForMarker TextMarkerId AnswerKind a
  | Backward                                a
  | Forward                                 a
  | GoTo                                    a
  | Init                                    a
  | HandleChange         PCM.CodeMirrorChange         (H.SubscribeStatus -> a)
  | HandleCursorActivity PCM.CodeMirrorCursorActivity (H.SubscribeStatus -> a)
  | HandleKey            KeyCombination               (H.SubscribeStatus -> a)

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
  modify (\ s -> s { nextMarkerId = nextMarkerId + 1 })
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
  , style do
      CSS.width           $ CSS.fromString "100%"
      CSS.height          $ CSS.fromString "20px"
      CSS.backgroundColor $ stageColor stage
  ]
  [ HH.text (show id) ]

render :: State -> H.ComponentHTML Query
render { code, cursorPosition, markers, tip } =
  HH.div_
    [ HH.div
      [ style $ do
           CSS.display CSS.flex
      ]
      (fromFoldable $ (renderMarker <<< _.marker) <$> Map.values markers)
    , HH.button [ HE.onClick $ HE.input_ Forward ]
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
  | CtrlAltRight
  | CtrlAltUp

type KeyBinding = { key :: KeyCombination, keyS :: String }

bindKey :: KeyCombination -> String -> KeyBinding
bindKey key keyS = { key, keyS }

keyBindings :: Array KeyBinding
keyBindings =
  [ bindKey CtrlAltDown  "Ctrl-Alt-Down"
  , bindKey CtrlAltRight "Ctrl-Alt-Right"
  , bindKey CtrlAltUp    "Ctrl-Alt-Up"
  ]

type CodeMirrorEffects e =
  ( avar    :: AVAR
  , console :: CONSOLE
  | e)

updateMarker ::
  ∀ e m. MonadAff (CodeMirrorEffects e) m =>
  TextMarkerId -> (TextMarker -> TextMarker) -> DSL m Unit
updateMarker markerId update = do
  H.modify $ over _markers $ Map.update (Just <<< over _marker update) markerId

-- | Hypothesis: we might not need to warn the parent component, because they only refer
-- | to the markerId, so they will just silently ignore all subsequent messages for this marker
-- | which is fine.
deleteMarker :: ∀ e m. MonadAff (CodeMirrorEffects e) m => TextMarkerId -> DSL m Unit
deleteMarker markerId = do
  H.gets (_.markers >>> Map.lookup markerId) >>= traverse_ \ { marker, reference } -> do
    H.liftEff $ PCM.clearTextMarker reference
  H.modify (over _markers $ Map.delete markerId)

getsDoc :: ∀ e m. MonadAff (CodeMirrorEffects e) m => DSL m (Maybe PCM.Doc)
getsDoc = gets _.codeMirror >>= traverse \ cm -> H.liftEff $ PCM.getDoc cm

eval :: ∀ e m. MonadAff (CodeMirrorEffects e) m => Query ~> DSL m
eval = case _ of

  AnswerForMarker markerId answer next ->
    case answer of

      Ack -> pure next

      Added sid loc added -> do
        -- H.liftEff $ log "Marking Processing"
        updateMarker markerId (_ { stage = Processing sid })
        pure next

      Completed -> pure next

      CoqExn mLoc mSids exn -> do
        -- this should really delete the marker, and all its followers
        deleteMarker markerId
        newTip <- H.gets (view _markers >>> Map.findMax) >>= case _ of
          Nothing         -> pure initialPosition
          Just lastMarker -> pure lastMarker.value.marker.to
        H.modify (_ { tip = newTip })
        getsDoc >>= traverse_ \ doc -> H.liftEff $ PCM.setCursor doc newTip Nothing
        pure next

      _ -> do
        H.liftEff $ log $ "TODO: AnswerForMarker: " <> show answer
        pure next

  Backward next -> do
    H.liftEff $ log $ "TODO: Backward"
    pure next

  Forward next -> do
    gets _.codeMirror >>= traverse_ \ cm -> do
      nextMarker >>= traverse_ \ marker -> do
        reference <- H.liftEff $ do
          doc <- PCM.getDoc cm
          let textMarkerOptions =
                Just $ TMO.def { css      = Just $ fold [ "background-color: "
                                                        , CSS.toHexString $ stageColor marker.stage
                                                        ]
                               , readOnly = Just true
                               }
          PCM.setCursor doc marker.to Nothing
          PCM.markText doc marker.from marker.to textMarkerOptions
        H.modify $ over _markers $ Map.insert marker.id { marker, reference }
        H.raise $ Sentence marker.id marker.sentence
    pure next

  GoTo next -> do
    H.liftEff $ log $ "TODO: GoTo"
    pure next

  Init next -> do
    { code } <- H.get
    H.getHTMLElementRef (H.RefLabel "codemirror") >>= traverse_ \ element -> do
      cm <- H.liftEff do
        PCM.codeMirror element
          (CFG.def { autofocus   = Just true
                   , lineNumbers = Just true
                   , mode        = Just "text/x-ocaml"
                   , value       = Just code
                   }
          )
      H.modify (_ { codeMirror = Just cm })
      subscribeTo (PCM.onCodeMirrorChange cm)         HandleChange
      subscribeTo (PCM.onCodeMirrorCursorActivity cm) HandleCursorActivity
      for_ keyBindings \ { key, keyS } -> do
        subscribeTo (PCM.addKeyMap cm keyS false) $ const (HandleKey key)
    pure next

  HandleCursorActivity o status -> do
    pos <- H.liftEff $ do
      doc          <- PCM.getDoc o.instance
      PCM.getCursor doc Nothing
    H.modify (_ { cursorPosition = pos })
    pure $ status H.Listening

  HandleChange change status -> do
    -- H.liftEff $ log $ "Change, removed: " <> change.changeObj.removed
    H.gets _.codeMirror >>= traverse_ \ cm -> do
      code <- H.liftEff $ do
        doc <- PCM.getDoc cm
        PCM.getValue doc Nothing
      H.modify (_ { code = code })
    pure $ status H.Listening

  HandleKey u status -> do
    case u of
      CtrlAltDown  -> eval $ H.action Forward
      CtrlAltRight -> eval $ H.action GoTo
      CtrlAltUp    -> eval $ H.action Backward
    pure $ status H.Listening

  where

    subscribeTo ::
      ∀ o.
      ((o -> Eff (CodeMirrorEffects e) Unit) -> Eff (CodeMirrorEffects e) Unit) ->
      (o -> ((H.SubscribeStatus -> H.SubscribeStatus) -> Query H.SubscribeStatus)) -> DSL m Unit
    subscribeTo es h = H.subscribe $ H.eventSource es (Just <<< H.request <<< h)

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
