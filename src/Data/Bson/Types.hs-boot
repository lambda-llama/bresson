module Data.Bson.Types where

import Control.DeepSeq (NFData)
import Data.Text (Text)

type Label = Text

data Value

instance Eq Value
instance Show Value
