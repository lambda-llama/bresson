{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Data.Bson.Instances () where

#include "MachDeps.h"

import Control.Applicative ((<$>))
import Data.Int (Int16, Int8, Int32, Int64)
import Data.Time.Clock (UTCTime)
import qualified Data.ByteString as S

import Data.Text (Text)
import Data.Text.Buildable (Buildable)
import Data.Text.Format (format)
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as Vector

import Data.Bson.Class (ToBson(..), FromBson(..))
import Data.Bson.Types (BsonValue(..), BsonBinary(..), BsonDocument)

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

instance FromBson BsonValue where
    fromBson = Right . id
    {-# INLINE fromBson #-}

instance FromBson BsonDocument where
    fromBson (BsonValueDocument d) = Right d
    fromBson _                     = Left "Expected BsonValueDocument"
    {-# INLINE fromBson #-}

instance FromBson Text where
    fromBson (BsonValueString t) = Right t
    fromBson _                   = Left "Expected BsonValueText"
    {-# INLINE fromBson #-}

instance FromBson S.ByteString where
    fromBson (BsonValueBinary (BsonBinaryGeneric b)) = Right b
    fromBson _ = Left "Expected BsonValueBinary BsonBinaryGeneric"
    {-# INLINE fromBson #-}

instance FromBson Bool where
    fromBson (BsonValueBool b) = Right b
    fromBson _                 = Left "Expected BsonValueBool"
    {-# INLINE fromBson #-}

instance FromBson Int8 where
    fromBson (BsonValueInt32 i) = bsonFromIntegral i
    fromBson (BsonValueInt64 i) = bsonFromIntegral i
    fromBson _ = Left "Expected BsonValueInt32 or BsonValueInt64"
    {-# INLINE fromBson #-}

instance FromBson Int16 where
    fromBson (BsonValueInt32 i) = bsonFromIntegral i
    fromBson (BsonValueInt64 i) = bsonFromIntegral i
    fromBson _ = Left "Expected BsonValueInt32 or BsonValueInt64"
    {-# INLINE fromBson #-}

instance FromBson Int32 where
    fromBson (BsonValueInt32 i) = Right i
    fromBson (BsonValueInt64 i) = bsonFromIntegral i
    fromBson _ = Left "Expected BsonValueInt32 or BsonValueInt64"
    {-# INLINE fromBson #-}

instance FromBson Int64 where
    fromBson (BsonValueInt32 i) = Right $ fromIntegral i
    fromBson (BsonValueInt64 i) = Right i
    fromBson _ = Left "Expected BsonValueInt32 or BsonValueInt64"
    {-# INLINE fromBson #-}

instance FromBson Int where
    fromBson (BsonValueInt32 i) = Right $ fromIntegral i
#if WORD_SIZE_IN_BITS == 32
    fromBson (BsonValueInt64 i) = bsonFromIntegral i
#elif WORD_SIZE_IN_BITS == 64
    fromBson (BsonValueInt64 i) = Right $ fromIntegral i
#endif
    fromBson _ = Left "Expected BsonValueInt32 or BsonValueInt64"
    {-# INLINE fromBson #-}

instance FromBson Double where
    fromBson (BsonValueDouble d) = Right d
    fromBson _ = Left "Expected BsonValueDouble"
    {-# INLINE fromBson #-}

instance FromBson UTCTime where
    fromBson (BsonValueUtcTime t) = Right t
    fromBson _ = Left "Expected BsonValueUtcTime"
    {-# INLINE fromBson #-}

instance FromBson a => FromBson (Maybe a) where
    fromBson BsonValueNull = Right Nothing
    fromBson v = Just <$> fromBson v
    {-# INLINE fromBson #-}

instance FromBson a => FromBson [a] where
    fromBson (BsonValueArray a) = Vector.toList <$> Vector.mapM fromBson a
    fromBson _ = Left "Expected BsonValueArray"
    {-# INLINE fromBson #-}

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
