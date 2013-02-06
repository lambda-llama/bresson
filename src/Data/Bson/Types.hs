{-# LANGUAGE DeriveDataTypeable #-}

module Data.Bson.Types
    ( BsonValue(..)
    , BsonBinary(..)
    , BsonObjectId(..)
    , BsonDocument
    , BsonArray
    , BsonLabel
    , BsonField
    ) where

import Data.Int (Int32, Int64)
import Data.Time.Clock (UTCTime)
import Data.Time.Format ()
import Data.Typeable (Typeable)
import Data.Word (Word32, Word16)
import qualified Data.ByteString as S

import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import Data.Word.Word24 (Word24)
import Data.Text (Text)

-- | A BSON value is one of the following types of values
data BsonValue = BsonValueDouble {-# UNPACK #-} !Double
               | BsonValueString Text
               | BsonValueDocument BsonDocument
               | BsonValueArray BsonArray
               | BsonValueBinary BsonBinary
               | BsonValueObjectId BsonObjectId
               | BsonValueBool Bool
               | BsonValueUtcTime UTCTime
               | BsonValueNull
               | BsonValueRegex Text Text
               | BsonValueJavascript Text
               | BsonValueJavascriptWithScope BsonDocument Text
               | BsonValueInt32 {-# UNPACK #-} !Int32
               | BsonValueInt64 {-# UNPACK #-} !Int64
               | BsonValueTimestamp {-# UNPACK #-} !Int64
               | BsonValueMin
               | BsonValueMax
    deriving (Eq, Show, Typeable)

type BsonLabel = Text
type BsonDocument = HashMap BsonLabel BsonValue
type BsonArray = Vector BsonValue
type BsonField = (BsonLabel, BsonValue)

data BsonObjectId = BsonObjectId
    { bsonObjectIdTime    :: {-# UNPACK #-} !Word32
    , bsonObjectIdMachine :: {-# UNPACK #-} !Word32
    , bsonObjectIdPid     :: {-# UNPACK #-} !Word16
    , bsonObjectIdInc     :: {-# UNPACK #-} !Word24
    } deriving (Eq, Show, Typeable)

data BsonBinary = BsonBinaryGeneric     S.ByteString
                | BsonBinaryFunction    S.ByteString
                | BsonBinaryUuid        S.ByteString
                | BsonBinaryMd5         S.ByteString
                | BsonBinaryUserDefined S.ByteString
    deriving (Eq, Show, Typeable)
