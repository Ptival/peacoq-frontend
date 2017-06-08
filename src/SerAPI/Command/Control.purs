module SerAPI.Command.Control where

import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import SerAPI.Command.ToSexp (class ToSexp, toSexp)
import SerAPI.Types (StateId, RouteId)

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

type QueryOptions =
  { route :: Maybe RouteId
  }

defaultQueryOptions :: QueryOptions
defaultQueryOptions =
  { route : Nothing
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
  | StmQuery { queryOptions :: QueryOptions
             , query        :: String
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
      let opts = fold [ toSexpOption "limit"  addOptions.limit
                      , toSexpOption "ontop"  addOptions.ontop
                      , toSexpOption "newtip" addOptions.newtip
                      , toSexpOption "verb"   addOptions.verb
                      ]
      in
      fold ["(StmAdd (", opts, ") ", toSexp sentence, ")"]

    StmCancel { stateIds } -> fold ["(StmCancel ", toSexp stateIds , ")"]

    StmObserve { stateId } -> fold ["(StmObserve ", toSexp stateId , ")"]

    StmQuery { queryOptions, query } ->
      let opts = fold [ toSexpOption "route" queryOptions.route
                      ]
      in
      fold ["(StmQuery (", opts, ") ", toSexp query, ")"]

    Quit -> "Quit"
