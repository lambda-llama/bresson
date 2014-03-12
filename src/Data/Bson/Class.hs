module Data.Bson.Class
    ( ToBson(..)
    , FromBson(..)
    ) where

import Data.Bson.Types (Value)
import Data.Bson.Parser (Parser)

class ToBson a where
    toBson :: a -> Value

class FromBson a where
    fromBson :: Value -> Parser a
