module CodeMirror.Position where

-- import Control.Monad.Error.Class
-- import Control.Monad.State.Class
import Prelude
import Data.Array as Array
import Data.String as String
import Data.String.Utils as String.Utils
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (Maybe(..))

newtype Position = Position
  { line :: Int
  , ch :: Int
  }

unPosition :: Position -> { line :: Int, ch :: Int }
unPosition (Position p) = p

derive instance genericPosition :: Generic Position

instance eqPosition :: Eq Position where
  eq = gEq

instance showPosition :: Show Position where
  show = gShow

initialPosition :: Position
initialPosition = Position
  { line : 0
  , ch : 0
  }

bumpLine :: Position -> Position
bumpLine (Position { line, ch }) = (Position { line : line + 1, ch : 0 })

bumpCh :: Position -> Position
bumpCh (Position { line, ch }) = (Position { line : line, ch : ch + 1 })

type PositionState =
  { docAfter  :: String
  , docBefore :: String
  , position  :: Position
  }

nextPositionState :: PositionState -> Maybe PositionState
nextPositionState { docAfter, docBefore, position } =
  if String.length docAfter == 0
  then Nothing
  else do
    { head, tail } <- String.uncons docAfter
    Just { docAfter  : tail
         , docBefore : docBefore <> String.singleton head
         , position  : if head == '\n' then bumpLine position else bumpCh position
         }

findFirstPositionWhere :: (String -> Boolean) -> PositionState -> Maybe PositionState
findFirstPositionWhere cond s =
  if cond s.docAfter
  then Just s
  else nextPositionState s >>= findFirstPositionWhere cond

stringStartsWithTerminator :: String -> Boolean
stringStartsWithTerminator s =
    String.Utils.startsWith ". " s
    || String.Utils.startsWith ".\n" s
    || s == "." -- captures the final period if there's no space or newline

stringAtPosition :: Position -> String -> Maybe String
stringAtPosition (Position { line, ch }) s =
  let ls = String.Utils.lines s in
  let remainingLines = Array.drop line ls in
  do
    ({ head, tail }) <- Array.uncons remainingLines
    if ch > String.length head
      then Nothing
      else Just (String.joinWith "\n" ((String.drop ch head) Array.: tail))

makePositionState :: String -> PositionState
makePositionState s =
  { docAfter  : s
  , docBefore : ""
  , position  : initialPosition
  }

moveToPosition :: Position -> PositionState -> Maybe PositionState
moveToPosition target s =
  if s.position == target
  then Just s
  else nextPositionState s >>= moveToPosition target

-- | Given a corpus and a position, find the next Coq terminator
nextPosition :: String -> Position -> Maybe Position
nextPosition code from = do
  psCurrentPosition <- moveToPosition from (makePositionState code)
  { position } <- findFirstPositionWhere stringStartsWithTerminator psCurrentPosition
  -- bumping to go after the period
  Just (bumpCh position)
