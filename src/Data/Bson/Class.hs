module Data.Bson.Class
    ( ToBson(..)
    , FromBson(..)
    ) where

import Data.Text (Text)

import Data.Bson.Types (Value)

class ToBson a where
    toBson :: a -> Value

class FromBson a where
    fromBson :: Value -> Either Text a
