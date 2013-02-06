module Data.Bson
    ( -- * Document
      BsonDocument
    , BsonValue(..)
    , BsonBinary(..)
    , BsonObjectId(..)
    , BsonLabel
    , ToBson(..)
    , FromBson(..)
    , document
    , (!?)
    , (=:)
) where

import Data.Bson.Class (ToBson(..), FromBson(..))
import Data.Bson.Types (BsonValue(..), BsonBinary(..),
                        BsonObjectId(..), BsonDocument, BsonLabel)
import Data.Bson.Utils (document, (!?), (=:))