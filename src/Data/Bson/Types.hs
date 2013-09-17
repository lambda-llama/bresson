{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Data.Bson.Types
    ( RegexOption(..)
    , RegexOptions
    , Value(..)
    , Binary(..)
    , ObjectId(..)
    , Document
    , Array
    , Label
    , Field
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

-- | Options for 'ValueRegex', constructors order is important because
-- it's binary representation should be encoded in alphabetical order.
data RegexOption = RegexOptionCaseInsensitive -- i
                 | RegexOptionLocaleDependent -- l
                 | RegexOptionMultiline       -- m
                 | RegexOptionDotall          -- s
                 | RegexOptionUnicode         -- u
                 | RegexOptionVerbose         -- x
    deriving (Eq, Show, Typeable, Enum)

type RegexOptions = BitSet RegexOption

-- | A  value is one of the following types of values
data Value = ValueDouble {-# UNPACK #-} !Double
           | ValueString {-# UNPACK #-} !Text
           | ValueDocument !Document
           | ValueArray {-# UNPACK #-} !Array
           | ValueBinary !Binary
           | ValueObjectId {-# UNPACK #-} !ObjectId
           | ValueBool !Bool
           | ValueUtcTime {-# UNPACK #-} !UTCTime
           | ValueNull
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 708)
           | ValueRegex {-# UNPACK #-} !Text {-# UNPACK #-} !RegexOptions
#else
           | ValueRegex {-# UNPACK #-} !Text !RegexOptions
#endif
           | ValueJavascript {-# UNPACK #-} !Text
           | ValueJavascriptWithScope {-# UNPACK #-} !Text !Document
           | ValueInt32 {-# UNPACK #-} !Int32
           | ValueInt64 {-# UNPACK #-} !Int64
           | ValueTimestamp {-# UNPACK #-} !Int64
           | ValueMin
           | ValueMax
    deriving (Eq, Show, Typeable)

type Label = Text
type Document = HashMap Label Value
type Array = Vector Value
type Field = (Label, Value)

data ObjectId = ObjectId
    { objectIdTime    :: {-# UNPACK #-} !Word32
    , objectIdMachine :: {-# UNPACK #-} !Word24
    , objectIdPid     :: {-# UNPACK #-} !Word16
    , objectIdInc     :: {-# UNPACK #-} !Word24
    } deriving (Eq, Show, Typeable)

data Binary = BinaryGeneric     {-# UNPACK #-} !S.ByteString
            | BinaryFunction    {-# UNPACK #-} !S.ByteString
            | BinaryUuid        {-# UNPACK #-} !UUID
            | BinaryMd5         {-# UNPACK #-} !S.ByteString
            | BinaryUserDefined {-# UNPACK #-} !S.ByteString
    deriving (Eq, Show, Typeable)
