{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Data.Bson.Instances () where

#include "MachDeps.h"

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData(..))
#if !MIN_VERSION_bytestring(0,10,0)
import Data.ByteString (ByteString)
#endif
import Data.Int (Int16, Int8, Int32, Int64)
import Data.Time.Clock (UTCTime)
import qualified Data.ByteString as S

import Data.Text (Text)
import Data.Text.Buildable (Buildable)
import Data.Text.Format (format)
import Data.Vector (Vector)
import Data.UUID (UUID)
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as Vector

import Data.Bson.Class (ToBson(..), FromBson(..))
import Data.Bson.Types (BsonValue(..), BsonBinary(..), BsonObjectId(..),
                        BsonDocument)

-------------------------------------------------------------------------------
-- * ToBson instances

instance ToBson BsonValue where
    toBson = id
    {-# INLINE toBson #-}

instance ToBson BsonDocument where
    toBson = BsonValueDocument
    {-# INLINE toBson #-}

instance ToBson Text where
    toBson = BsonValueString
    {-# INLINE toBson #-}

instance ToBson S.ByteString where
    toBson = BsonValueBinary . BsonBinaryGeneric
    {-# INLINE toBson #-}

instance ToBson Bool where
    toBson = BsonValueBool
    {-# INLINE toBson #-}

instance ToBson Int8 where
    toBson = BsonValueInt32 . fromIntegral
    {-# INLINE toBson #-}

instance ToBson Int16 where
    toBson = BsonValueInt32 . fromIntegral
    {-# INLINE toBson #-}

instance ToBson Int32 where
    toBson = BsonValueInt32
    {-# INLINE toBson #-}

instance ToBson Int64 where
    toBson = BsonValueInt64
    {-# INLINE toBson #-}

instance ToBson Int where
#if WORD_SIZE_IN_BITS == 32
    toBson = BsonValueInt32 . fromIntegral
#elif WORD_SIZE_IN_BITS == 64
    toBson i
        | i `within` int32Bounds = BsonValueInt32 $ fromIntegral i
        | otherwise              = BsonValueInt64 $ fromIntegral i
      where
        int32Bounds = ( fromIntegral (minBound :: Int32)
                      , fromIntegral (maxBound :: Int32)
                      )
#endif
    {-# INLINE toBson #-}

instance ToBson Double where
    toBson = BsonValueDouble
    {-# INLINE toBson #-}

instance ToBson UTCTime where
    toBson = BsonValueUtcTime
    {-# INLINE toBson #-}

instance ToBson a => ToBson (Maybe a) where
    toBson (Just a) = toBson a
    toBson Nothing  = BsonValueNull
    {-# INLINE toBson #-}

instance ToBson a => ToBson [a] where
    toBson = BsonValueArray . Vector.fromList . map toBson
    {-# INLINE toBson #-}

instance ToBson a => ToBson (Vector a) where
    toBson = BsonValueArray . Vector.map toBson
    {-# INLINE toBson #-}

-------------------------------------------------------------------------------
-- * FromBson instances

unexpectedBsonValue :: Text -> BsonValue -> Text
unexpectedBsonValue expected v =
    LT.toStrict $ format "Expected {}, got {} instead" [expected, got]
  where
    got :: Text
    got = case v of
        BsonValueDocument _d -> "Embedded document"
        BsonValueString _s   -> "UTF-8 string"
        BsonValueBinary _b   -> "Binary"
        BsonValueBool _b     -> "Boolean"
        BsonValueInt32 _i    -> "32-bit Integer"
        BsonValueInt64 _i    -> "64-bit Integer"
        BsonValueDouble _d   -> "Double"
        BsonValueUtcTime _t  -> "UTC datetime"
        BsonValueArray _a    -> "Array"
        BsonValueObjectId {} -> "ObjectId"
        BsonValueNull        -> "NULL"
        BsonValueRegex _p _o -> "Regular expression"
        BsonValueJavascript _c -> "JavaScript code"
        BsonValueJavascriptWithScope _c _s -> "JavaScript code with scope"
        BsonValueTimestamp _i  -> "Timestamp"
        BsonValueMin         -> "Min key"
        BsonValueMax         -> "Max key"

{-# INLINE unexpectedBsonValue #-}

instance FromBson BsonValue where
    fromBson = Right . id
    {-# INLINE fromBson #-}

instance FromBson BsonDocument where
    fromBson (BsonValueDocument d) = Right d
    fromBson v = Left $ unexpectedBsonValue "Embedded document" v
    {-# INLINE fromBson #-}

instance FromBson Text where
    fromBson (BsonValueString t) = Right t
    fromBson v = Left $ unexpectedBsonValue "UTF-8 string" v
    {-# INLINE fromBson #-}

instance FromBson S.ByteString where
    fromBson (BsonValueBinary (BsonBinaryGeneric b)) = Right b
    fromBson v = Left $ unexpectedBsonValue "Binary" v
    {-# INLINE fromBson #-}

instance FromBson Bool where
    fromBson (BsonValueBool b) = Right b
    fromBson v                 = Left $ unexpectedBsonValue "Boolean" v
    {-# INLINE fromBson #-}

instance FromBson Int8 where
    fromBson (BsonValueInt32 i) = bsonFromIntegral i
    fromBson (BsonValueInt64 i) = bsonFromIntegral i
    fromBson v = Left $ unexpectedBsonValue "32-bit or 64-bit Integer" v
    {-# INLINE fromBson #-}

instance FromBson Int16 where
    fromBson (BsonValueInt32 i) = bsonFromIntegral i
    fromBson (BsonValueInt64 i) = bsonFromIntegral i
    fromBson v = Left $ unexpectedBsonValue "32-bit or 64-bit Integer" v
    {-# INLINE fromBson #-}

instance FromBson Int32 where
    fromBson (BsonValueInt32 i) = Right i
    fromBson (BsonValueInt64 i) = bsonFromIntegral i
    fromBson v = Left $ unexpectedBsonValue "32-bit or 64-bit Integer" v
    {-# INLINE fromBson #-}

instance FromBson Int64 where
    fromBson (BsonValueInt32 i) = Right $ fromIntegral i
    fromBson (BsonValueInt64 i) = Right i
    fromBson v = Left $ unexpectedBsonValue "32-bit or 64-bit Integer" v
    {-# INLINE fromBson #-}

instance FromBson Int where
    fromBson (BsonValueInt32 i) = Right $ fromIntegral i
#if WORD_SIZE_IN_BITS == 32
    fromBson (BsonValueInt64 i) = bsonFromIntegral i
#elif WORD_SIZE_IN_BITS == 64
    fromBson (BsonValueInt64 i) = Right $ fromIntegral i
#endif
    fromBson v = Left $ unexpectedBsonValue "32-bit or 64-bit Integer" v
    {-# INLINE fromBson #-}

instance FromBson Double where
    fromBson (BsonValueDouble d) = Right d
    fromBson v = Left $ unexpectedBsonValue "Double" v
    {-# INLINE fromBson #-}

instance FromBson UTCTime where
    fromBson (BsonValueUtcTime t) = Right t
    fromBson v = Left $ unexpectedBsonValue "UTC datetime" v
    {-# INLINE fromBson #-}

instance FromBson a => FromBson (Maybe a) where
    fromBson BsonValueNull = Right Nothing
    fromBson v = Just <$> fromBson v
    {-# INLINE fromBson #-}

instance FromBson a => FromBson [a] where
    fromBson (BsonValueArray a) = Vector.toList <$> Vector.mapM fromBson a
    fromBson v = Left $ unexpectedBsonValue "Array" v
    {-# INLINE fromBson #-}

instance FromBson a => FromBson (Vector a) where
    fromBson (BsonValueArray a) = Vector.mapM fromBson a
    fromBson v = Left $ unexpectedBsonValue "Array" v
    {-# INLINE fromBson #-}

-------------------------------------------------------------------------------
-- * NFData instances for internal types

instance NFData BsonValue where
    rnf (BsonValueDouble _) = ()
    rnf (BsonValueString a) = rnf a `seq` ()
    rnf (BsonValueDocument a) = rnf a `seq` ()
    rnf (BsonValueArray a) = rnf a `seq` ()
    rnf (BsonValueBinary a) = rnf a `seq` ()
    rnf (BsonValueObjectId a) = rnf a `seq` ()
    rnf (BsonValueBool _) = ()
    rnf (BsonValueUtcTime a) = rnf a `seq` ()
    rnf BsonValueNull = ()
    rnf (BsonValueRegex a b) = rnf a `seq` rnf b `seq` ()
    rnf (BsonValueJavascript a) = rnf a `seq` ()
    rnf (BsonValueJavascriptWithScope a b) = rnf a `seq` rnf b `seq` ()
    rnf (BsonValueInt32 _) = ()
    rnf (BsonValueInt64 _) = ()
    rnf (BsonValueTimestamp _) = ()
    rnf BsonValueMin = ()
    rnf BsonValueMax = ()

instance NFData BsonObjectId where
    rnf (BsonObjectId {}) = ()

instance NFData BsonBinary where
    rnf (BsonBinaryGeneric a) = rnf a `seq` ()
    rnf (BsonBinaryFunction a) = rnf a `seq` ()
    rnf (BsonBinaryUuid a) = rnf a `seq` ()
    rnf (BsonBinaryMd5 a) = rnf a `seq` ()
    rnf (BsonBinaryUserDefined a) = rnf a `seq` ()

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData ByteString where
    rnf x = S.length x `seq` ()
#endif

instance NFData UUID where
    rnf x = x `seq` ()

-------------------------------------------------------------------------------
-- * Helpers

bsonFromIntegral :: forall a b. (Integral a, Integral b, Bounded b, Buildable a)
                 => a
                 -> Either Text b
bsonFromIntegral i
    | i `within` targetBounds = Right $ fromIntegral i
    | otherwise =
        Left . LT.toStrict $
        format "Integral {} out of bounds [{}, {}]" [i, targetMin, targetMax]
  where
    targetBounds = ( fromIntegral (minBound :: b)
                   , fromIntegral (maxBound :: b)
                   )
    (targetMin, targetMax) = targetBounds
{-# INLINE bsonFromIntegral #-}

-- | Make sure a given value is within a @[low, high]@ bound.
within :: Ord a => a -> (a, a) -> Bool
within x (l, h) = x >= l && x <= h
{-# INLINE within #-}
