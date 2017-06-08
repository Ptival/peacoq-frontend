module CodeMirror.Component where

import Prelude
import CSS as CSS
import CSS.Display as CSSD
import CSS.Overflow as CSSO
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ports.CodeMirror as PCM
import Ports.CodeMirror.Configuration as CFG
import Stage as Stage
import CodeMirror.Position (Position, Strictness(Strictly), addPosition, initialPosition, isBefore)
import CodeMirror.TextMarker (TextMarker, TextMarkerId)
import Control.Apply (lift2)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Loops (whileM)
import Control.Monad.State.Class (class MonadState, gets, modify)
import Coq.Position (nextSentence)
import Data.Array (elem, fromFoldable)
import Data.Foldable (find)
import Data.Lens (lens, over, view)
import Data.Lens.Types (Lens')
import Data.Maybe (Maybe(..))
import Data.String (take)
import Data.Traversable (for_, traverse, traverse_)
import Halogen.HTML.CSS (style)
import Halogen.Query (RefLabel(..))
import SerAPI.Answer (AnswerKind(..))
import SerAPI.Feedback (EditOrStateId(..), Feedback(..), FeedbackContent(..), Level(..))
import SerAPI.Types (StateId)

markerBarHeight :: ∀ a. CSS.Size a
markerBarHeight = CSS.fromString "20px"

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
  = Backward                                        a
  | Forward                                         a
  | GoTo                                            a
  | Init                                            a
  | ProcessAnswer   AnswerKind (Maybe TextMarkerId) a
  | ProcessFeedback Feedback                        a
  | HandleChange         PCM.CodeMirrorChange         (H.SubscribeStatus -> a)
  | HandleCursorActivity PCM.CodeMirrorCursorActivity (H.SubscribeStatus -> a)
  | HandleKey            KeyCombination               (H.SubscribeStatus -> a)

data Message
  = Cancel StateId
  | Sentence TextMarkerId String

addMarker :: ∀ m. MonadState State m => Position -> Position -> String -> m TextMarker
addMarker from to sentence = do
  nextMarkerId <- gets _.nextMarkerId
  let newMarker = { id       : nextMarkerId
                  , from
                  , to
                  , sentence
                  , stage    : Stage.ToProcess
                  }
  modify (\ s -> s { nextMarkerId = nextMarkerId + 1 })
  pure newMarker

nextTip ::
  ∀ e m.
  MonadAff (CodeMirrorEffects e) m =>
  DSL m (Maybe { position :: Position, sentence :: String })
nextTip = do
  maybeResult <- getsDoc >>= traverse \ doc -> do
    tip <- getsTip
    nbLines <- H.liftEff $ PCM.lineCount doc
    code <- H.liftEff $ PCM.getRange doc tip { line : nbLines + 1, ch : 0 } Nothing
    H.liftEff $ log $ "Suffix seems to be: " <> take 20 code
    pure $ do
      r <- nextSentence code { line : 0, ch : 0 }
      -- at this point, `r.position` is the position within the code suffix, not the global one
      pure $ r { position = addPosition tip r.position }
  pure $ join maybeResult

nextMarker ::
  ∀ e m.
  MonadAff (CodeMirrorEffects e) m =>
  DSL m (Maybe TextMarker)
nextMarker = do
  nextTip >>= traverse \ { position : newTip, sentence } -> do
    tip <- getsTip
    newMarker <- addMarker tip newTip sentence
    modify (\ s -> s { tip = newMarker.to })
    pure newMarker

renderMarker :: TextMarker -> H.ComponentHTML Query
renderMarker { id, stage } =
  HH.div
  [ HP.title $ show id
  , style do
      flexCol
      CSS.backgroundColor $ Stage.stageColor stage
      CSS.border          CSS.solid (CSS.fromString "1px") CSS.black
      CSS.boxSizing       $ CSS.borderBox
      CSS.height          $ CSS.fromString "100%"
      CSS.width           $ CSS.fromString "100%"
  ]
  [ HH.text (show id) ]

flex :: CSS.StyleM Unit
flex = do
  CSS.alignContent  $ CSS.stretch
  CSS.alignItems    $ CSS.stretch
  CSS.display       $ CSS.flex

flexCol :: CSS.StyleM Unit
flexCol = do
  flex
  CSS.flexDirection $ CSS.column

flexRow :: CSS.StyleM Unit
flexRow = do
  flex
  CSS.flexDirection $ CSS.row

render :: State -> H.ComponentHTML Query
render { code, cursorPosition, markers, tip } =
  HH.div [ style $ do
              flexCol
              CSS.height    $ CSS.fromString "100%"
              CSS.minHeight $ CSS.fromString "100%"
         ]
  $
  [ HH.div [ style $ do
                flexRow
                CSS.flexGrow $ 2
           ]
    $
    [ HH.div
      [ style $ do
           flexCol
           CSS.width $ CSS.fromString "50%"
      ]
      [ HH.div
        [ HP.ref (RefLabel "codemirror")
        , style do
             CSSD.float    $ CSSD.floatLeft
             CSS.height    $ CSS.fromString "100%"
             CSS.minHeight $ CSS.fromString "100%"
        ]
        [ -- intentionally left empty, will be filled by CodeMirror
        ]
      ]
    ]
    <>
    [ HH.div
      [ style do
           flexCol
           CSS.width     $ CSS.fromString "50%"
      ]
      [ HH.pre
        [ style $ do
             CSS.height       $ CSS.fromString "100%"
             CSS.minHeight    $ CSS.fromString "100%"
             CSS.marginBottom $ CSS.fromString "0"
             CSS.marginTop    $ CSS.fromString "0"
             CSSO.overflowY   $ CSSO.overflowAuto
        ]
        [ HH.text code ]
      ]
    ]
  ]
  <>
  [ HH.div
    [ style $ do
         flexRow
         CSS.height    $ markerBarHeight
         CSS.minHeight $ markerBarHeight
         CSS.maxHeight $ markerBarHeight
    ]
    (fromFoldable $ (renderMarker <<< _.marker) <$> Map.values markers)
  ]
    -- [ HH.button [ HE.onClick $ HE.input_ Forward ]
    --             [ HH.text "+" ]
    -- , HH.p_ [ HH.text ("Line is: " <> show cursorPosition.line)]
    -- ]

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
  getsDoc >>= traverse_ \ doc -> do
    H.gets (view _markers >>> Map.lookup markerId) >>= traverse_ \ { marker, reference } -> do
      let newMarker = update marker
      newReference <- H.liftEff $ do
        PCM.clearTextMarker reference
        PCM.markText doc marker.from marker.to (Stage.textMarkerOptions newMarker.stage)
      H.modify $ over _markers $ Map.insert markerId { marker : newMarker, reference : newReference }
  pure unit

-- | Hypothesis: we might not need to warn the parent component, because they only refer
-- | to the markerId, so they will just silently ignore all subsequent messages for this marker
-- | which is fine.
deleteMarker :: ∀ e m. MonadAff (CodeMirrorEffects e) m => TextMarkerId -> DSL m Unit
deleteMarker markerId = do
  H.gets (_.markers >>> Map.lookup markerId) >>= traverse_ \ { marker, reference } -> do
    -- H.liftEff $ log $ "Asking CodeMirror to clear the marker"
    H.liftEff $ PCM.clearTextMarker reference
  -- H.liftEff $ log $ "Now removing the marker from the Map"
  H.modify (over _markers $ Map.delete markerId)

deleteMarkerUpdatingTip :: ∀ e m. MonadAff (CodeMirrorEffects e) m => TextMarkerId -> DSL m Unit
deleteMarkerUpdatingTip markerId = do
  deleteMarker markerId
  newTip <- H.gets (view _markers >>> Map.findMax) >>= case _ of
    Nothing         -> pure initialPosition
    Just lastMarker -> pure lastMarker.value.marker.to
  H.modify (_ { tip = newTip })
  getsDoc >>= traverse_ \ doc -> H.liftEff $ PCM.setCursor doc newTip Nothing

getsDoc :: ∀ e m. MonadAff (CodeMirrorEffects e) m => DSL m (Maybe PCM.Doc)
getsDoc = gets _.codeMirror >>= traverse \ cm -> H.liftEff $ PCM.getDoc cm

getsTip :: ∀ e m. MonadAff (CodeMirrorEffects e) m => DSL m Position
getsTip = gets _.tip

getsProcessingMarkerWithStateId ::
  ∀ e m. MonadAff (CodeMirrorEffects e) m => StateId -> DSL m (Maybe TextMarker)
getsProcessingMarkerWithStateId sid =
  H.gets $
  view _markers
  >>> find (\ m -> m.marker.stage == Stage.Processing sid)
  >>> liftA1 _.marker

eval :: ∀ e m. MonadAff (CodeMirrorEffects e) m => Query ~> DSL m
eval = case _ of

  Backward next -> do
    gets (_.markers >>> Map.findMax) >>= traverse_ \ { key, value } -> do
      Stage.stageStateId value.marker.stage # traverse_ \ sid -> H.raise $ Cancel sid
    pure next

  Forward next -> do
    gets _.codeMirror >>= traverse_ \ cm -> do
      nextMarker >>= traverse_ \ marker -> do
        reference <- H.liftEff do
          doc <- PCM.getDoc cm
          PCM.setCursor doc marker.to Nothing
          PCM.markText doc marker.from marker.to (Stage.textMarkerOptions marker.stage)
        H.modify $ over _markers $ Map.insert marker.id { marker, reference }
        H.raise $ Sentence marker.id marker.sentence
    pure next

  GoTo next -> do
    target <- gets _.cursorPosition
    gets _.codeMirror >>= traverse_ \ cm -> do
      doc <- H.liftEff $ PCM.getDoc cm
      sentences <- whileM
        (lift2 (isBefore Strictly) getsTip (pure target))
        do
          nextMarker >>= traverse \ marker -> do
            reference <- H.liftEff do
              PCM.markText doc marker.from marker.to (Stage.textMarkerOptions marker.stage)
            H.modify $ over _markers $ Map.insert marker.id { marker, reference }
            pure $ Sentence marker.id marker.sentence
      H.liftEff $ log "Now raising sentences"
      for_ sentences (_ # traverse_ H.raise)
    pure next

  Init next -> do
    { code } <- H.get
    H.getHTMLElementRef (H.RefLabel "codemirror") >>= traverse_ \ element -> do
      cm <- H.liftEff do
        PCM.codeMirror element
          (CFG.def { autofocus      = Just true
                   , lineNumbers    = Just true
                   , mode           = Just "text/x-ocaml"
                   , value          = Just code
                   }
          )
      H.liftEff $ PCM.setSize cm "100%" "100%"
      H.modify (_ { codeMirror = Just cm })
      subscribeTo (PCM.onCodeMirrorChange cm)         HandleChange
      subscribeTo (PCM.onCodeMirrorCursorActivity cm) HandleCursorActivity
      for_ keyBindings \ { key, keyS } -> do
        subscribeTo (PCM.addKeyMap cm keyS false) $ const (HandleKey key)
    pure next

  ProcessAnswer answer maybeMarkerId next ->
    case answer of

      Ack -> pure next

      Added sid loc added -> do
        case maybeMarkerId of
          Nothing       -> H.liftEff $ log $ "Received Added for an unknown marker id"
          Just markerId -> updateMarker markerId (_ { stage = Stage.Processing sid })
        pure next

      Canceled stateIds-> do
        markers <- gets (view _markers)
        for_ markers \ { marker, reference } -> do
          if marker.stage == Stage.ToProcess || elem (Stage.stageStateId marker.stage) (Just <$> stateIds)
            then deleteMarker marker.id
            else pure unit
          pure unit
        newTip <- gets (view _markers >>> Map.findMax) >>= case _ of
          Nothing         -> pure initialPosition
          Just lastMarker -> pure lastMarker.value.marker.to
        H.modify (_ { tip = newTip })
        getsDoc >>= traverse_ \ doc -> H.liftEff $ PCM.setCursor doc newTip Nothing
        pure next

      Completed -> pure next

      CoqExn mLoc mSids exn -> do
        -- It often happens that we receive a CoqExn for an unknown marker id, usually for
        -- a stmObserve that failed
        maybeMarkerId # traverse_ deleteMarkerUpdatingTip
        pure next

      _ -> do
        H.liftEff $ log $ "TODO: AnswerForMarker: " <> show answer
        pure next

  ProcessFeedback (Feedback { id, contents, route }) next -> do
    case id of
      EditId  eid -> pure next
      StateId sid ->
        case contents of
          Processed -> do
            getsProcessingMarkerWithStateId sid >>= traverse_ \ marker -> do
              updateMarker marker.id (_ { stage = Stage.Processed sid })
            pure next
          Message Error _ _ -> do
            getsProcessingMarkerWithStateId sid >>= traverse_ \ marker -> do
              deleteMarkerUpdatingTip marker.id
            pure next
          _ -> pure next

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
