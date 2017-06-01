module SerAPI.Types where

import Prelude
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))
import Ports.Sexp (Sexp(..))
import SerAPI.FromSexp (class FromSexp, fromSexp)

type EditId = Int
type StateId = Int

data CoqObject = CoqObject

derive instance genericCoqObject :: Generic CoqObject

instance showCoqObject :: Show CoqObject where
  show = gShow

data Loc =
  Loc
  { fname      :: String
  , lineNb     :: Int
  , bolPos     :: Int
  , lineNbLast :: Int
  , bolPosLast :: Int
  , bp         :: Int
  , ep         :: Int
  }

instance fromSexpLoc :: FromSexp Loc where
  fromSexp = case _ of
    List [ List [ Atom "fname"
                , sfname
                ]
         , List [ Atom "line_nb"
                , slineNb
                ]
         , List [ Atom "bol_pos"
                , sbolPos
                ]
         , List [ Atom "line_nb_last"
                , slineNbLast
                ]
         , List [Atom "bol_pos_last"
                , sbolPosLast
                ]
         , List [ Atom "bp"
                , sbp
                ]
         , List [ Atom "ep"
                , sep
                ]
         ]
      -> do
      fname      <- fromSexp sfname
      lineNb     <- fromSexp slineNb
      bolPos     <- fromSexp sbolPos
      lineNbLast <- fromSexp slineNbLast
      bolPosLast <- fromSexp sbolPosLast
      bp         <- fromSexp sbp
      ep         <- fromSexp sep
      pure $ Loc { fname, lineNb, bolPos, lineNbLast, bolPosLast, bp, ep }
    _ -> Nothing

derive instance genericLoc :: Generic Loc

instance showLoc :: Show Loc where
  show = gShow

data Exn = Exn Sexp

derive instance genericExn :: Generic Exn

instance showExn :: Show Exn where
  show = gShow

instance fromSexpExn :: FromSexp Exn where
  fromSexp = Just <<< Exn
