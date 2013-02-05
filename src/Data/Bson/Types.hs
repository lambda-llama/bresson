{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Bson.Types
    ( BsonValue(..)
    , BsonBinary(..)
    , BsonObjectId(..)
    , BsonDocument
    , BsonArray
    , BsonLabel
    ) where

import Data.Int (Int32, Int64)
import Data.Time.Clock (UTCTime)
import Data.Time.Format ()
import Data.Word (Word32, Word16)
import qualified Data.ByteString as S

import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import Data.Word.Word24 (Word24)
import Data.Text (Text)

-- | A BSON value is one of the following types of values
data BsonValue = BsonValueDouble Double
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
               | BsonValueInt32 Int32
               | BsonValueInt64 Int64
               | BsonValueTimestamp Int64
               | BsonValueMin
               | BsonValueMax
            deriving (Eq, Show)

type BsonLabel = Text
type BsonDocument = HashMap BsonLabel BsonValue
type BsonArray = Vector BsonValue

data BsonObjectId = BsonObjectId
    { bsonObjectIdTime    :: Word32
    , bsonObjectIdMachine :: Word32
    , bsonObjectIdPid     :: Word16
    , bsonObjectIdInc     :: Word24
    } deriving (Eq, Show)

data BsonBinary = BsonBinaryGeneric     S.ByteString
                | BsonBinaryFunction    S.ByteString
                | BsonBinaryUuid        S.ByteString
                | BsonBinaryMd5         S.ByteString
                | BsonBinaryUserDefined S.ByteString
    deriving (Eq, Show)
