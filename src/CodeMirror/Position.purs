module CodeMirror.Position where

-- import Control.Monad.Error.Class
-- import Control.Monad.State.Class
import Prelude
import Data.Array as Array
import Data.String as String
import Data.String.Utils as String.Utils
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))

newtype Position = Position
  { line :: Int
  , ch :: Int
  }

unPosition :: Position -> { line :: Int, ch :: Int }
unPosition (Position p) = p

derive instance genericPosition :: Generic Position

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

-- nextPosition :: forall m. MonadThrow Unit m => MonadState PositionState m => m Unit
-- nextPosition = pure unit

addPos :: Position -> Position -> Position
addPos (Position pos1) (Position pos2) = Position
    { line : pos1.line + pos2.line
    , ch : if pos2.line == 0 then pos1.ch + pos2.ch else pos2.ch
    }

findFirstPositionWhere :: (String -> Boolean) -> String -> Maybe Position
findFirstPositionWhere cond s0 = aux s0 (Position { line : 0, ch : 0 })
  where
    aux :: String -> Position -> Maybe Position
    aux "" pos = Nothing
    aux s  pos =
      if cond s then
        Just pos
      else do
        { head, tail } <- String.uncons s
        aux s (if head == '\n' then bumpLine pos else bumpCh pos)

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

-- | Given a corpus and a position, find the next Coq terminator
nextPosition :: String -> Position -> Maybe Position
nextPosition code (pos1@(Position { line, ch })) = do
  remainingCode <- stringAtPosition pos1 code
  pos2 <- findFirstPositionWhere stringStartsWithTerminator remainingCode
  let finalPos = addPos pos1 pos2
  -- bumping to go after the period
  Just (bumpCh finalPos)
