module Ports.CodeMirror.Configuration where

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)

type ConfigurationF f =
  { autofocus     :: f Boolean
  --, extraKeys     :: Nullable Foreign
  , lineNumbers   :: f Boolean
  , lineSeparator :: f String
  , mode          :: f String
  , value         :: f String
  }

type RawConfiguration = ConfigurationF Nullable
type Configuration    = ConfigurationF Maybe

toRaw :: Configuration -> RawConfiguration
toRaw c =
  { autofocus     : toNullable c.autofocus
  --, extraKeys     : toNullable (options <$> c.extraKeys)
  , lineNumbers   : toNullable c.lineNumbers
  , lineSeparator : toNullable c.lineSeparator
  , mode          : toNullable c.mode
  , value         : toNullable c.value
  }

def :: Configuration
def =
  { autofocus     : Nothing
  -- , extraKeys     : Nothing
  , lineNumbers   : Nothing
  , lineSeparator : Nothing
  , mode          : Nothing
  , value         : Nothing
  }
