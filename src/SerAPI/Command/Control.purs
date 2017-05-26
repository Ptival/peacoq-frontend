module SerAPI.Command.Control where

import Data.List (List)
import Data.Maybe (Maybe(..))
import Prelude ((<>))
import SerAPI.Command.ToSexp (class ToSexp, toSexp)

type StateId = Int

type AddOptions =
  { limit  :: Maybe Int
  , ontop  :: Maybe StateId
  , newtip :: Maybe StateId
  , verb   :: Maybe Boolean
  }

data Control
  = LibAdd     { qualifiedPath :: List String
               , physicalPath  :: String
               , containsML    :: Boolean
               }
  | StmAdd     { addOptions :: AddOptions
               , sentence   :: String
               }
  | StmCancel  { stateIds :: List StateId
               }
  | StmObserve { stateId :: StateId
               }
  | Quit

toSexpOption :: ∀ t. ToSexp t => String -> Maybe t -> String
toSexpOption option = case _ of
  Nothing -> ""
  Just v  -> "(" <> option <> " " <> toSexp v <> ")"

instance toSexpControl :: ToSexp Control where
  toSexp = case _ of

    LibAdd r -> "TODO"

    StmAdd { addOptions, sentence } ->
      let { limit, ontop, newtip, verb } = addOptions in
      let opts
            =  toSexpOption "limit"  limit
            <> toSexpOption "ontop"  ontop
            <> toSexpOption "newtip" newtip
            <> toSexpOption "verb"   verb
      in
      "(StmAdd (" <> opts <> ") <> " <> toSexp sentence <> ")"

    StmCancel { stateIds } -> "TODO"

    StmObserve { stateId } -> "TODO"

    Quit -> "TODO"
