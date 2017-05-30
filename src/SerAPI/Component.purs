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
import SerAPI.Command (Command)
import SerAPI.Command.ToSexp (toSexp)
import SerAPI.Feedback (Feedback)
import SerAPI.FromSexp (fromSexp)

type SerAPIEffects e =
  ( ajax    :: AX.AJAX
  , console :: CONSOLE
  | e)

type Input =
  {}

type State =
  { commandCounter :: Int
  , lastCommand    :: Maybe Command
  }

data Query a
  = Ping         a
  | Send Command a

data Message
  = Answer Answer
  | Feedback Feedback

derive instance genericMessage :: Generic Message

instance showMessage :: Show Message where
  show = gShow

render :: State -> ComponentHTML Query
render { lastCommand } =
  HH.div_
  [ HH.text $ fold [ "SerAPI is loaded, last command: "
                   , fromMaybe "no command sent yet" (toSexp <$> lastCommand)
                   ]
  ]

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

send :: ∀ e m. MonadAff (SerAPIEffects e) m => Command -> DSL m (AX.AffjaxResponse String)
send = H.liftAff <<< affjaxSerAPI "coqtop" <<< Just <<< toSexp

serAPIOutput :: String -> Maybe Message
serAPIOutput o = do
  s <- sexp o
  oneOf $
    [ Answer <$> fromSexp s
    , Feedback <$> fromSexp s
    ]

handleResponse :: ∀ e m. MonadAff (SerAPIEffects e) m => AX.AffjaxResponse String -> m Unit
handleResponse r = do
  for_ (jsonParseArray r.response) \ responses -> do
    for responses \ response -> do
      case serAPIOutput response of
        Just m -> do
          --H.liftEff $ log $ "Correctly read message: " <> show m
          pure unit
        Nothing -> do
          H.liftEff $ log $ "Could not decode: " <> (show $ sexp response)
    
eval :: ∀ e m. MonadAff (SerAPIEffects e) m => Query ~> DSL m
eval = case _ of

  Ping next -> do
    ping >>= handleResponse
    pure next

  Send cmd next -> do
    H.liftEff $ log $ "SerAPI: sending command" <> toSexp cmd
    H.modify (_ { lastCommand = Just cmd })
    send cmd >>= handleResponse
    pure next

serAPIComponent ::
  ∀ e m. MonadAff (SerAPIEffects e) m => H.Component HH.HTML Query Input Message m
serAPIComponent = H.lifecycleComponent
  { initialState : const { commandCounter : 0
                         , lastCommand : Nothing
                         }
  , render
  , eval
  , receiver     : const Nothing
  , initializer  : Nothing
  , finalizer    : Nothing
  }