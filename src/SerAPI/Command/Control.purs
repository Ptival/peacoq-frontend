module SerAPI.Command.Control where

import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import SerAPI.Command.ToSexp (class ToSexp, toSexp)
import SerAPI.Types (StateId)

type AddOptions =
  { limit  :: Maybe Int
  , ontop  :: Maybe StateId
  , newtip :: Maybe StateId
  , verb   :: Maybe Boolean
  }

defaultAddOptions :: AddOptions
defaultAddOptions =
  { limit  : Nothing
  , ontop  : Nothing
  , newtip : Nothing
  , verb   : Nothing
  }

data Control
  = LibAdd     { qualifiedPath :: Array String
               , physicalPath  :: String
               , containsML    :: Boolean
               }
  | StmAdd     { addOptions :: AddOptions
               , sentence   :: String
               }
  | StmCancel  { stateIds :: Array StateId
               }
  | StmObserve { stateId :: StateId
               }
  | Quit

toSexpOption :: ∀ t. ToSexp t => String -> Maybe t -> String
toSexpOption option = case _ of
  Nothing -> ""
  Just v  -> fold ["(", option, " ", toSexp v, ")"]

instance toSexpControl :: ToSexp Control where
  toSexp = case _ of

    LibAdd r -> "TODO"

    StmAdd { addOptions, sentence } ->
      let { limit, ontop, newtip, verb } = addOptions in
      let opts = fold [ toSexpOption "limit"  limit
                      , toSexpOption "ontop"  ontop
                      , toSexpOption "newtip" newtip
                      , toSexpOption "verb"   verb
                      ]
      in
      fold ["(StmAdd (", opts, ") ", toSexp sentence, ")"]

    StmCancel { stateIds } -> fold ["(StmCancel ", toSexp stateIds , ")"]

    StmObserve { stateId } -> fold ["(StmObserve ", toSexp stateId , ")"]

    Quit -> "Quit"
