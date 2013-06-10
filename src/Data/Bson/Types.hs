{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Data.Bson.Types
    ( BsonRegexOption(..)
    , BsonRegexOptions
    , BsonValue(..)
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

import Data.BitSet.Word (BitSet)
import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import Data.Word.Word24 (Word24)
import Data.Text (Text)
import Data.UUID (UUID)

-- | Options for 'BsonValueRegex', constructors order is important because
-- it's binary representation should be encoded in alphabetical order.
data BsonRegexOption = BsonRegexOptionCaseInsensitive -- i
                     | BsonRegexOptionLocaleDependent -- l
                     | BsonRegexOptionMultiline       -- m
                     | BsonRegexOptionDotall          -- s
                     | BsonRegexOptionUnicode         -- u
                     | BsonRegexOptionVerbose         -- x
    deriving (Eq, Show, Typeable, Enum)

type BsonRegexOptions = BitSet BsonRegexOption

-- | A BSON value is one of the following types of values
data BsonValue = BsonValueDouble {-# UNPACK #-} !Double
               | BsonValueString {-# UNPACK #-} !Text
               | BsonValueDocument !BsonDocument
               | BsonValueArray {-# UNPACK #-} !BsonArray
               | BsonValueBinary !BsonBinary
               | BsonValueObjectId {-# UNPACK #-} !BsonObjectId
               | BsonValueBool !Bool
               | BsonValueUtcTime {-# UNPACK #-} !UTCTime
               | BsonValueNull
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 708)
               | BsonValueRegex {-# UNPACK #-} !Text {-# UNPACK #-} !BsonRegexOptions
#else
               | BsonValueRegex {-# UNPACK #-} !Text !BsonRegexOptions
#endif
               | BsonValueJavascript {-# UNPACK #-} !Text
               | BsonValueJavascriptWithScope {-# UNPACK #-} !Text !BsonDocument
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
    , bsonObjectIdMachine :: {-# UNPACK #-} !Word24
    , bsonObjectIdPid     :: {-# UNPACK #-} !Word16
    , bsonObjectIdInc     :: {-# UNPACK #-} !Word24
    } deriving (Eq, Show, Typeable)

data BsonBinary = BsonBinaryGeneric     {-# UNPACK #-} !S.ByteString
                | BsonBinaryFunction    {-# UNPACK #-} !S.ByteString
                | BsonBinaryUuid        {-# UNPACK #-} !UUID
                | BsonBinaryMd5         {-# UNPACK #-} !S.ByteString
                | BsonBinaryUserDefined {-# UNPACK #-} !S.ByteString
    deriving (Eq, Show, Typeable)
