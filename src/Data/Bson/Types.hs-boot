module Data.Bson.Types where

import Data.Text (Text)

type Label = Text

data Value

instance Eq Value
instance Show Value
