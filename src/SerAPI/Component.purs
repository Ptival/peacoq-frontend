module SerAPI.Component where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Network.HTTP.Affjax as AX
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen.Component (ComponentHTML, ComponentDSL)
import SerAPI.Command (Command)
import SerAPI.Command.ToSexp (toSexp)

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

data Message = Void

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

ping :: ∀ e m. MonadAff (SerAPIEffects e) m => DSL m (AX.AffjaxResponse String)
ping = H.liftAff $ affjaxSerAPI "ping" Nothing

send :: ∀ e m. MonadAff (SerAPIEffects e) m => Command -> DSL m (AX.AffjaxResponse String)
send = H.liftAff <<< affjaxSerAPI "coqtop" <<< Just <<< toSexp

eval :: ∀ e m. MonadAff (SerAPIEffects e) m => Query ~> DSL m
eval = case _ of

  Ping next -> do
    response <- ping
    H.liftEff $ log $ "Received ping response: " <> response.response
    pure next

  Send cmd next -> do
    H.liftEff $ log $ "SerAPI: sending command" <> toSexp cmd
    response <- send cmd 
    H.liftEff $ log $ "Received send response: " <> response.response
    H.modify (_ { lastCommand = Just cmd })
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
