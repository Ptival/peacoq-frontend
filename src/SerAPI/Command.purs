module SerAPI.Command where

import SerAPI.Command.Control as C
import Data.Foldable (fold)
import SerAPI.Command.ToSexp (class ToSexp, toSexp)

data Command
  = Control C.Control

instance toSexpCommand :: ToSexp Command where
  toSexp = case _ of
    Control c -> fold ["(Control ", toSexp c, ")"]

type CommandTag = Int

data TaggedCommand = TaggedCommand CommandTag Command

instance toSexpTaggedCommand :: ToSexp TaggedCommand where
  toSexp (TaggedCommand tag cmd) = fold ["(", toSexp tag, " ", toSexp cmd, ")"]

tagOf :: TaggedCommand -> CommandTag
tagOf (TaggedCommand tag _) = tag
