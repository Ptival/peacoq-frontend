module SerAPI.Component where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Network.HTTP.Affjax as AX
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.Foldable (fold, oneOf)
import Data.Generic (class Generic, gShow)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for, for_)
import Halogen.Component (ComponentHTML, ComponentDSL)
import Ports.Sexp (jsonParseArray, sexp)
import SerAPI.Answer (Answer)
import SerAPI.Command (Command(Control), CommandTag, TaggedCommand(TaggedCommand))
import SerAPI.Command.Control (Control(..))
import SerAPI.Command.ToSexp (toSexp)
import SerAPI.Feedback (Feedback)
import SerAPI.FromSexp (fromSexp)

type SerAPIEffects e =
  ( ajax    :: AX.AJAX
  , console :: CONSOLE
  | e)

type Input = Unit

type CommandId = Int

type State =
  { nextCommandTag  :: CommandTag
  , lastCommandSent :: Maybe TaggedCommand
  }

data Query a
  = Init               a
  | Ping               a
  | Reset              a
  | Send TaggedCommand a
  | TagCommand Command (TaggedCommand -> a)

data Message
  = Answer Answer
  | Feedback Feedback

derive instance genericMessage :: Generic Message

instance showMessage :: Show Message where
  show = gShow

render :: State -> ComponentHTML Query
render { lastCommandSent } = HH.div_ []
{-
  HH.div_
  [ HH.text $ fold [ "SerAPI is loaded, last command: "
                   , fromMaybe "no command sent yet" (toSexp <$> lastCommandSent)
                   ]
  ]
-}

type DSL = ComponentDSL State Query Message

affjaxSerAPI :: ∀ e. String -> Maybe String -> AX.Affjax e String
affjaxSerAPI url content = AX.affjax
  { method          : Left POST
  , url
  , headers         : []
  , content         : content
  , username        : Nothing
  , password        : Nothing
  , withCredentials : false
  }

readList :: String -> Maybe (Array String)
readList = jsonParseArray

ping :: ∀ e m. MonadAff (SerAPIEffects e) m => DSL m (AX.AffjaxResponse String)
ping = H.liftAff $ affjaxSerAPI "ping" Nothing

send :: ∀ e m. MonadAff (SerAPIEffects e) m => TaggedCommand -> DSL m (AX.AffjaxResponse String)
send tcmd = do
  H.modify (_ { lastCommandSent = Just tcmd })
  H.liftAff <<< affjaxSerAPI "coqtop" <<< Just <<< toSexp $ tcmd

serAPIOutput :: String -> Maybe Message
serAPIOutput o = do
  s <- sexp o
  oneOf $
    [ Answer <$> fromSexp s
    , Feedback <$> fromSexp s
    ]

handleResponse :: ∀ e m. MonadAff (SerAPIEffects e) m => AX.AffjaxResponse String -> DSL m Unit
handleResponse r = do
  for_ (jsonParseArray r.response) \ responses -> do
    for responses \ response -> do
      case serAPIOutput response of
        Just m -> do
          H.raise m
          -- H.liftEff $ log $ "Correctly read message: " <> show m
          pure unit
        Nothing -> do
          H.liftEff $ log $ "Could not decode: " <> (show $ sexp response)

newCommandTag :: ∀ m. DSL m CommandTag
newCommandTag = do
  t <- H.gets _.nextCommandTag
  H.modify (_ { nextCommandTag = t + 1 })
  pure t

tagCommand :: ∀ m. Command -> DSL m TaggedCommand
tagCommand cmd = do
  tag <- newCommandTag
  pure $ TaggedCommand tag cmd

eval :: ∀ e m. MonadAff (SerAPIEffects e) m => Query ~> DSL m
eval = case _ of

  Init next -> do
    eval $ H.action $ Reset
    pure next

  Ping next -> do
    -- H.liftEff $ log "Ping"
    ping >>= handleResponse
    -- H.liftEff $ log "Done ping"
    pure next

  TagCommand cmd next -> do
    tcmd <- tagCommand cmd
    pure $ next $ tcmd

  Reset next -> do
    tcmd <- eval $ H.request $ TagCommand $ Control $ Quit
    eval $ H.action $ Send $ tcmd
    pure next

  Send tcmd next -> do
    -- H.liftEff $ log $ "SerAPI: sending command" <> toSexp tcmd
    send tcmd >>= handleResponse
    pure $ next

serAPIComponent ::
  ∀ e m. MonadAff (SerAPIEffects e) m => H.Component HH.HTML Query Input Message m
serAPIComponent = H.lifecycleComponent
  { initialState : const { nextCommandTag  : 0
                         , lastCommandSent : Nothing
                         }
  , render
  , eval
  , receiver     : const Nothing
  , initializer  : Just $ H.action Init
  , finalizer    : Nothing
  }
