module CodeMirror.Position where

import Prelude
import Data.Array as Array
import Data.String as String
import Data.String.Utils as String.Utils
import Control.Apply (applySecond, lift2)
import Control.Monad.State.Class (class MonadState, get, gets, modify, put)
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)

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

-- | Moves the position state by one character, returns the character that was after and is now before
forward :: ∀ m. MonadState PositionState m => m (Maybe Char)
forward = do
  { docAfter, docBefore, position } <- get
  if String.length docAfter == 0
    then pure Nothing
    else do
      String.uncons docAfter # traverse \ { head, tail } -> do
        modify (\ s -> s { docAfter  = tail
                         , docBefore = docBefore <> String.singleton head
                         , position  = if head == '\n' then bumpLine position else bumpCh position
                         })
        pure head

-- | Returns the position at which the document suffix satisfies a condition
findFirstPositionWhere :: ∀ m. MonadState PositionState m => (String -> Boolean) -> m (Maybe Position)
findFirstPositionWhere cond = do
  { docAfter, position } <- get
  if cond docAfter
    then pure $ Just position
    else forward `applySecond` findFirstPositionWhere cond

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

moveToPosition :: ∀ m. MonadState PositionState m => Position -> m Unit
moveToPosition target = do
  p <- gets _.position
  if p == target
    then pure unit
    else forward *> moveToPosition target

-- | `peek n` lets you peek `n` characters into the document, not modifying the state
peek :: ∀ m. MonadState PositionState m => Int -> m String
peek n0 = do
  s <- get
  r <- go n0
  put s
  pure r
  where
    go n = if n == 0
           then pure ""
           else lift2 squash forward (peek (n - 1))
    squash Nothing  s = s
    squash (Just c) s = String.singleton c <> s

reachedDocEnd :: ∀ m. MonadState PositionState m => m Boolean
reachedDocEnd = do
  s <- gets _.docAfter
  pure (String.length s == 0)
