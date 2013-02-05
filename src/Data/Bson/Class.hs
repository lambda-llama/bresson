module Data.Bson.Class
    ( ToBson(..)
    , FromBson(..)
    ) where

import Data.Text(Text)

import Data.Bson.Types (BsonValue)

class ToBson a where
    toBson :: a -> BsonValue

class FromBson a where
    fromBson :: BsonValue -> Either Text a
