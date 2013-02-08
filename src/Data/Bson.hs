module Data.Bson
    ( -- * Document
      BsonDocument
    , BsonLabel
    , BsonValue(..)
    , BsonBinary(..)
    , BsonObjectId(..)
    , BsonArray
    , ToBson(..)
    , FromBson(..)
    , document
    , (!?)
    , (=:)
) where

import Data.Bson.Class (ToBson(..), FromBson(..))
import Data.Bson.Types (BsonDocument, BsonLabel, BsonValue(..),
                        BsonBinary(..), BsonObjectId(..), BsonArray)
import Data.Bson.Utils (document, (!?), (=:))
