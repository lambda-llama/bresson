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
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Time.Clock (UTCTime)
import qualified Data.ByteString as S

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.Bson.Class (ToBson(..), FromBson(..))
import Data.Bson.Parser (Parser)
import Data.Bson.Types (Value(..), Binary(..), ObjectId(..),
                        Document)

-------------------------------------------------------------------------------
-- * ToBson instances

instance ToBson Value where
    toBson = id
    {-# INLINE toBson #-}

instance ToBson Document where
    toBson = ValueDocument
    {-# INLINE toBson #-}

instance ToBson Text where
    toBson = ValueString
    {-# INLINE toBson #-}

instance ToBson S.ByteString where
    toBson = ValueBinary . BinaryGeneric
    {-# INLINE toBson #-}

instance ToBson Bool where
    toBson = ValueBool
    {-# INLINE toBson #-}

instance ToBson Int8 where
    toBson = ValueInt32 . fromIntegral
    {-# INLINE toBson #-}

instance ToBson Int16 where
    toBson = ValueInt32 . fromIntegral
    {-# INLINE toBson #-}

instance ToBson Int32 where
    toBson = ValueInt32
    {-# INLINE toBson #-}

instance ToBson Int64 where
    toBson = ValueInt64
    {-# INLINE toBson #-}

instance ToBson Int where
#if WORD_SIZE_IN_BITS == 32
    toBson = ValueInt32 . fromIntegral
#elif WORD_SIZE_IN_BITS == 64
    toBson i
        | i `within` int32Bounds = ValueInt32 $ fromIntegral i
        | otherwise              = ValueInt64 $ fromIntegral i
      where
        int32Bounds = ( fromIntegral (minBound :: Int32)
                      , fromIntegral (maxBound :: Int32)
                      )
#endif
    {-# INLINE toBson #-}

instance ToBson Double where
    toBson = ValueDouble
    {-# INLINE toBson #-}

instance ToBson UTCTime where
    toBson = ValueUtcTime
    {-# INLINE toBson #-}

instance ToBson a => ToBson (Maybe a) where
    toBson (Just a) = toBson a
    toBson Nothing  = ValueNull
    {-# INLINE toBson #-}

instance ToBson a => ToBson [a] where
    toBson = ValueArray . Vector.fromList . map toBson
    {-# INLINE toBson #-}

instance ToBson a => ToBson (Vector a) where
    toBson = ValueArray . Vector.map toBson
    {-# INLINE toBson #-}

-------------------------------------------------------------------------------
-- * FromBson instances

unexpectedValue :: String -> Value -> String
unexpectedValue expected v =
    "Expected " ++ show expected ++ " got " ++ show got ++ " instead"
  where
    got :: String
    got = case v of
        ValueDocument _d -> "Embedded document"
        ValueString _s   -> "UTF-8 string"
        ValueBinary _b   -> "Binary"
        ValueBool _b     -> "Boolean"
        ValueInt32 _i    -> "32-bit Integer"
        ValueInt64 _i    -> "64-bit Integer"
        ValueDouble _d   -> "Double"
        ValueUtcTime _t  -> "UTC datetime"
        ValueArray _a    -> "Array"
        ValueObjectId {} -> "ObjectId"
        ValueNull        -> "NULL"
        ValueRegex _p _o -> "Regular expression"
        ValueJavascript _c -> "JavaScript code"
        ValueJavascriptWithScope _c _s -> "JavaScript code with scope"
        ValueTimestamp _i  -> "Timestamp"
        ValueMin         -> "Min key"
        ValueMax         -> "Max key"
{-# INLINE unexpectedValue #-}

instance FromBson Value where
    fromBson = return . id
    {-# INLINE fromBson #-}

instance FromBson Document where
    fromBson (ValueDocument d) = return d
    fromBson v = fail $ unexpectedValue "Embedded document" v
    {-# INLINE fromBson #-}

instance FromBson Text where
    fromBson (ValueString t) = return t
    fromBson v = fail $ unexpectedValue "UTF-8 string" v
    {-# INLINE fromBson #-}

instance FromBson S.ByteString where
    fromBson (ValueBinary (BinaryGeneric b)) = return b
    fromBson v = fail $ unexpectedValue "Binary" v
    {-# INLINE fromBson #-}

instance FromBson Bool where
    fromBson (ValueBool b) = return b
    fromBson v                 = fail $ unexpectedValue "Boolean" v
    {-# INLINE fromBson #-}

instance FromBson Int8 where
    fromBson (ValueInt32 i) = bsonFromIntegral i
    fromBson (ValueInt64 i) = bsonFromIntegral i
    fromBson v = fail $ unexpectedValue "32-bit or 64-bit Integer" v
    {-# INLINE fromBson #-}

instance FromBson Int16 where
    fromBson (ValueInt32 i) = bsonFromIntegral i
    fromBson (ValueInt64 i) = bsonFromIntegral i
    fromBson v = fail $ unexpectedValue "32-bit or 64-bit Integer" v
    {-# INLINE fromBson #-}

instance FromBson Int32 where
    fromBson (ValueInt32 i) = return i
    fromBson (ValueInt64 i) = bsonFromIntegral i
    fromBson v = fail $ unexpectedValue "32-bit or 64-bit Integer" v
    {-# INLINE fromBson #-}

instance FromBson Int64 where
    fromBson (ValueInt32 i) = return $ fromIntegral i
    fromBson (ValueInt64 i) = return i
    fromBson v = fail $ unexpectedValue "32-bit or 64-bit Integer" v
    {-# INLINE fromBson #-}

instance FromBson Int where
    fromBson (ValueInt32 i) = return $ fromIntegral i
#if WORD_SIZE_IN_BITS == 32
    fromBson (ValueInt64 i) = bsonFromIntegral i
#elif WORD_SIZE_IN_BITS == 64
    fromBson (ValueInt64 i) = return $ fromIntegral i
#endif
    fromBson v = fail $ unexpectedValue "32-bit or 64-bit Integer" v
    {-# INLINE fromBson #-}

instance FromBson Double where
    fromBson (ValueDouble d) = return d
    fromBson v = fail $ unexpectedValue "Double" v
    {-# INLINE fromBson #-}

instance FromBson UTCTime where
    fromBson (ValueUtcTime t) = return t
    fromBson v = fail $ unexpectedValue "UTC datetime" v
    {-# INLINE fromBson #-}

instance FromBson a => FromBson (Maybe a) where
    fromBson ValueNull = return Nothing
    fromBson v = Just <$> fromBson v
    {-# INLINE fromBson #-}

instance FromBson a => FromBson [a] where
    fromBson (ValueArray a) = Vector.toList <$> Vector.mapM fromBson a
    fromBson v = fail $ unexpectedValue "Array" v
    {-# INLINE fromBson #-}

instance FromBson a => FromBson (Vector a) where
    fromBson (ValueArray a) = Vector.mapM fromBson a
    fromBson v = fail $ unexpectedValue "Array" v
    {-# INLINE fromBson #-}

-------------------------------------------------------------------------------
-- * NFData instances for internal types

instance NFData Value where
    rnf (ValueDouble _) = ()
    rnf (ValueString a) = rnf a `seq` ()
    rnf (ValueDocument a) = rnf a `seq` ()
    rnf (ValueArray a) = rnf a `seq` ()
    rnf (ValueBinary a) = rnf a `seq` ()
    rnf (ValueObjectId a) = rnf a `seq` ()
    rnf (ValueBool _) = ()
    rnf (ValueUtcTime a) = rnf a `seq` ()
    rnf ValueNull = ()
    rnf (ValueRegex a b) = rnf a `seq` rnf b `seq` ()
    rnf (ValueJavascript a) = rnf a `seq` ()
    rnf (ValueJavascriptWithScope a b) = rnf a `seq` rnf b `seq` ()
    rnf (ValueInt32 _) = ()
    rnf (ValueInt64 _) = ()
    rnf (ValueTimestamp _) = ()
    rnf ValueMin = ()
    rnf ValueMax = ()

instance NFData ObjectId where
    rnf (ObjectId {}) = ()

instance NFData Binary where
    rnf (BinaryGeneric a) = rnf a `seq` ()
    rnf (BinaryFunction a) = rnf a `seq` ()
    rnf (BinaryUuid a) = rnf a `seq` ()
    rnf (BinaryMd5 a) = rnf a `seq` ()
    rnf (BinaryUserDefined a) = rnf a `seq` ()

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData ByteString where
    rnf x = S.length x `seq` ()
#endif

-------------------------------------------------------------------------------
-- * Helpers

bsonFromIntegral :: forall a b. (Show a, Integral a, Integral b, Bounded b)
                 => a
                 -> Parser b
bsonFromIntegral i
    | i `within` targetBounds = return $ fromIntegral i
    | otherwise =
        fail $
        "Integral " ++ show i ++ " out of bounds [" ++ show targetMin ++ ", " ++ show targetMax ++ "]"
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
