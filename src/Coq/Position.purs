module Coq.Position where

import Prelude
import CodeMirror.Position (Position, PositionState, forward, makePositionState, moveToPosition, peek, reachedDocEnd)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (class MonadState, State, gets, runState)
import Control.Monad.State.Class (get, modify)
import Control.Monad.State.Trans (StateT)
import Data.Char.Unicode (isAlphaNum, isSpace)
import Data.Lens (lens, over)
import Data.Lens.Types (Lens')
import Data.Lens.Zoom (zoom)
import Data.List (List(..), elem, length, tail, uncons, (:))
import Data.List.Types (List)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (charAt, singleton)
import Data.String.Utils (startsWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

data Delimiters
  = OpenComment
  | OpenString

type NextSentenceState =
  { delimiterStack :: List Delimiters
  , positionState  :: PositionState
  , seenCharacter  :: Boolean
  , sentence       :: String
  }

_delimiterStack :: Lens' NextSentenceState (List Delimiters)
_delimiterStack = lens _.delimiterStack (_ { delimiterStack = _ })

_positionState :: Lens' NextSentenceState PositionState
_positionState = lens _.positionState (_ { positionState = _ })

_seenCharacter :: Lens' NextSentenceState Boolean
_seenCharacter = lens _.seenCharacter (_ { seenCharacter = _ })

_sentence :: Lens' NextSentenceState String
_sentence = lens _.sentence (_ { sentence = _ })

forward' :: ∀ m. Monad m => StateT NextSentenceState m (Maybe Char)
forward' = do
  stackIsEmpty <- gets (\ { delimiterStack } -> length delimiterStack == 0)
  zoom _positionState forward >>= traverse \ c -> do
    modify (over _sentence (_ <> singleton c))
    -- I wonder whether I want `isAlphaNum` or `not isSpace` or something else here...
    when (stackIsEmpty && isAlphaNum c) $ modify (over _seenCharacter (const true))
    pure c

peek' :: ∀ m. Monad m => Int -> StateT NextSentenceState m String
peek' n = zoom _positionState (peek n)

moveToPosition' :: ∀ m. MonadRec m => Position -> StateT NextSentenceState m Unit
moveToPosition' p = zoom _positionState (moveToPosition p)

-- | helpers to modify the stack
openComment :: ∀ m. MonadState NextSentenceState m => m Unit
openComment = modify (over _delimiterStack (OpenComment : _))

openString :: ∀ m. Monad m => StateT NextSentenceState m Unit
openString  = modify (over _delimiterStack (OpenString : _))

-- | note that closeComment and closeString do not check that they pop the right thing
closeComment :: ∀ m. Monad m => StateT NextSentenceState m Unit
closeComment = modify (over _delimiterStack (\ s -> fromMaybe s (tail s)))

closeString :: ∀ m. Monad m => StateT NextSentenceState m Unit
closeString  = modify (over _delimiterStack (\ s -> fromMaybe s (tail s)))

makeNextSentenceState :: PositionState -> NextSentenceState
makeNextSentenceState ps =
  { delimiterStack : Nil
  , positionState  : ps
  , seenCharacter  : false
  , sentence       : ""
  }

nextSentence :: String -> Position -> Maybe { sentence :: String, position :: Position }
nextSentence string position =
  let Tuple msentence state = runState (moveToPosition' position *> tailRecM go0 unit) initialState in
  msentence >>= \ sentence -> pure { sentence, position : state.positionState.position }

  where

    initialState :: NextSentenceState
    initialState = makeNextSentenceState (makePositionState string)

    go :: State NextSentenceState (Step Unit (Maybe String))
    go = pure $ Loop unit

    -- | assumptions:
    -- | - comments look like (* this *) and must be properly nested
    -- | - strings look like "this"
    -- | - strings within comments count as strings, so if they contain *) it does not close the comment
    -- | - sentences end with a period followed by some space/newline
    -- | - bullets are +, -, *, and can be juxtaposed without spacing
    -- | - bullet braces are { and }, and can be juxtaposed without spacing
    go0 :: Unit -> State NextSentenceState (Step Unit (Maybe String))
    go0 unit = do
      zoom _positionState reachedDocEnd >>= case _ of
        true -> pure $ Done $ Nothing
        false -> do
          { delimiterStack } <- get
          case uncons delimiterStack of
            -- if we are not within delimiters, we're looking for a period not followed by a character
            Nothing -> peek' 3 >>= peeked
            Just { head, tail } ->
              case head of
                -- if we're within a "(*", look for its "*)", or nested "(*"
                OpenComment -> do
                  s <- peek' 2
                  case s of
                    "(*" -> openComment *> forward' *> forward' *> go
                    "*)" -> forward' *> forward' *> closeComment *> go
                    _    -> forward' *> when (charAt 0 s == Just '"') openString *> go
                -- if we're within an open quote, look for its closing quote
                OpenString -> do
                  s <- peek' 2
                  case s of
                    -- within a string, an escaped quote like \" is ignored
                    "\\\""                          -> forward' *> forward' *> go
                    _      | charAt 0 s == Just '"' -> forward' *> closeString *> go
                    _                               -> forward' *> go

    peeked :: String -> State NextSentenceState (Step Unit (Maybe String))
    peeked s | startsWith "(*"   s = openComment *> forward' *> forward' *> go
             | startsWith "..."  s = forward' *> forward' *> forward' *> here
             | startsWith ".."   s = forward' *> forward' *> go
             | startsWith "."    s
             , followedBySpace   s = forward' *> here
             | startsWithABullet s = do
               seenCharacter <- gets _.seenCharacter
               forward' *> if seenCharacter then go else here
             | startsWith "\""   s = openString *> forward' *> go
             | otherwise           = forward' *> go

    followedBySpace :: String -> Boolean
    followedBySpace s = fromMaybe true (isSpace <$> charAt 1 s)

    startsWithABullet :: String -> Boolean
    startsWithABullet s = fromMaybe false (flip elem ['+', '-', '*', '{', '}'] <$> charAt 0 s)

    here :: State NextSentenceState (Step Unit (Maybe String))
    here = Done <<< Just <$> gets _.sentence
