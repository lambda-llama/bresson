module Data.Bson
    ( module Data.Bson.Class
    , module Data.Bson.Types
    , module Data.Bson.Utils
    ) where

-- Instance Binary BsonDocument
import Data.Bson.Binary ()

import Data.Bson.Class (ToBson(..), FromBson(..))
import Data.Bson.Types (BsonDocument, BsonLabel, BsonValue(..),
                        BsonBinary(..), BsonObjectId(..), BsonArray,
                        BsonRegexOption(..), BsonRegexOptions)
import Data.Bson.Utils (document, (!?), (=:))
