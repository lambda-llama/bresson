{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Data.Bson.Binary () where

#include "bson.h"

import Prelude hiding (length, concat)
import Control.Applicative ((<$>), (<*>))
import Data.Binary (Binary(..))
import Data.Binary.Get (Get, runGet, getWord8, getWord16le,
                        getWord32le, getWord64le, getLazyByteStringNul,
                        getLazyByteString, getByteString, lookAhead)
import Data.Binary.Put (Put, runPut, putWord8, putWord16le, putWord32le,
                        putWord64le, putLazyByteString, putByteString)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Text.Printf (printf)

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Data.Binary.IEEE754 (getFloat64le, putFloat64le)
import Data.Text (Text)
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Word.Word24 (Word24)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as Vector

import Data.Bson.Types (BsonDocument, BsonArray, BsonLabel, BsonValue(..),
                        BsonBinary(..), BsonObjectId(..))

instance Binary BsonDocument where
    put = putBsonDocument
    get = getBsonDocument

putBsonField :: BsonLabel -> BsonValue -> Put
putBsonField key value = do
    putWord8 tag
    putBsonCString key
    payload
  where
    (tag, payload) = case value of
        BsonValueDouble a     -> (BSON_DOUBLE, putFloat64le a)
        BsonValueString a     -> (BSON_STRING, putBsonString a)
        BsonValueDocument a   -> (BSON_DOCUMENT, putBsonDocument a)
        BsonValueArray a      -> (BSON_ARRAY, putBsonArray a)
        BsonValueBinary a     -> (BSON_BINARY, putBsonBinary a)
        BsonValueObjectId a   -> (BSON_OID, putBsonObjectId a)
        BsonValueBool False   -> (BSON_BOOLEAN, putWord8 0)
        BsonValueBool True    -> (BSON_BOOLEAN, putWord8 1)
        BsonValueUtcTime a    -> (BSON_UTC, putBsonUtcTime a)
        BsonValueNull         -> (BSON_NULL, return ())
        BsonValueRegex a b    ->
            (BSON_REGEX, putBsonCString a >> putBsonCString b)
        BsonValueJavascript a -> (BSON_JS, putBsonCString a)
        BsonValueJavascriptWithScope a b ->
            (BSON_JS_WITH_SCOPE, putBsonJsWithScope a b)
        BsonValueInt32 a      -> (BSON_INT32, putWord32le $ fromIntegral a)
        BsonValueInt64 a      -> (BSON_INT64, putWord64le $ fromIntegral a)
        BsonValueTimestamp a  -> (BSON_TIMESTAMP, putWord64le $ fromIntegral a)
        BsonValueMin          -> (BSON_MIN, return ())
        BsonValueMax          -> (BSON_MAX, return ())
{-# INLINE putBsonField #-}

getBsonField :: Get (BsonLabel, BsonValue)
getBsonField = do
    tag <- getWord8
    key <- getBsonCString
    value <- case tag of
        BSON_DOUBLE    -> BsonValueDouble <$> getFloat64le
        BSON_STRING    -> BsonValueString <$> getBsonString
        BSON_DOCUMENT  -> BsonValueDocument <$> getBsonDocument
        BSON_ARRAY     -> BsonValueArray <$> getBsonArray
        BSON_BINARY    -> BsonValueBinary <$> getBsonBinary
        BSON_OID       -> BsonValueObjectId <$> getBsonObjectId
        BSON_BOOLEAN   -> BsonValueBool <$> getBsonBool
        BSON_UTC       -> BsonValueUtcTime <$> getBsonUtcTime
        BSON_NULL      -> return BsonValueNull
        BSON_REGEX     -> BsonValueRegex <$> getBsonCString <*> getBsonCString
        BSON_JS        -> BsonValueJavascript <$> getBsonCString
        BSON_JS_WITH_SCOPE ->
            uncurry BsonValueJavascriptWithScope <$> getBsonJsWithScope
        BSON_INT32     -> BsonValueInt32 . fromIntegral <$> getWord32le
        BSON_INT64     -> BsonValueInt64 . fromIntegral <$> getWord64le
        BSON_TIMESTAMP -> BsonValueTimestamp . fromIntegral <$> getWord64le
        BSON_MIN       -> return BsonValueMin
        BSON_MAX       -> return BsonValueMax
        _              -> fail $ printf "Invalid BSON tag: %i" tag
    return (key, value)
{-# INLINE getBsonField #-}

putAsDocument :: Put -> Put
putAsDocument p = do
    -- include length and null terminator
    putWord32le $ fromIntegral $ L.length bytes + 5
    putLazyByteString bytes
    putWord8 0x00
  where
    bytes = runPut p
{-# INLINE putAsDocument #-}

putBsonDocument :: BsonDocument -> Put
putBsonDocument doc = putAsDocument $ sequence_ $ HashMap.foldlWithKey' f [] doc
  where
    f a k v = putBsonField k v : a
{-# INLINE putBsonDocument #-}

getBsonDocument :: Get BsonDocument
getBsonDocument = do
    l <- fromIntegral <$> getWord32le
    bytes <- getLazyByteString $ l - 4
    return $ HashMap.fromList $ runGet getFields bytes
 where
    getFields = lookAhead getWord8 >>= \done -> if done == 0
        then return []
        else (:) <$> getBsonField
                 <*> getFields
{-# INLINE getBsonDocument #-}

intToText :: Int -> Text
intToText = TL.toStrict . TL.toLazyText . decimal
{-# INLINE intToText #-}

putBsonArray :: BsonArray -> Put
putBsonArray array = putAsDocument $ Vector.zipWithM_ putBsonField inf array
  where
    inf = Vector.generate (Vector.length array) intToText
{-# INLINE putBsonArray #-}

getBsonArray :: Get BsonArray
getBsonArray = getBsonDocument >>= \doc ->
    return $ Vector.unfoldr (f doc (HashMap.size doc)) 0
  where
    f doc s c | c >= s     = Nothing
              | otherwise = HashMap.lookup (intToText c) doc >>=
                     Just . (, c + 1)
{-# INLINE getBsonArray #-}

putBsonBinary :: BsonBinary -> Put
putBsonBinary binary = do
    putWord32le $ fromIntegral $ S.length payload
    putWord8 tag
    putByteString payload
  where
    (tag, payload) = case binary of
        BsonBinaryGeneric a     -> (BSON_GENERIC_BINARY, a)
        BsonBinaryFunction a    -> (BSON_FUNCTION, a)
        BsonBinaryUuid a        -> (BSON_UUID, a)
        BsonBinaryMd5 a         -> (BSON_MD5, a)
        BsonBinaryUserDefined a -> (BSON_USER_DEFINED_BINARY, a)
{-# INLINE putBsonBinary #-}

getBsonBinary :: Get BsonBinary
getBsonBinary = do
    len <- fromIntegral <$> getWord32le
    tag <- getWord8
    bytes <- getByteString len
    case tag of
        BSON_GENERIC_BINARY -> return $ BsonBinaryGeneric bytes
        BSON_FUNCTION -> return $ BsonBinaryFunction bytes
        BSON_UUID -> return $ BsonBinaryUuid bytes
        BSON_MD5 -> return $ BsonBinaryMd5 bytes
        BSON_USER_DEFINED_BINARY -> return $ BsonBinaryUserDefined bytes
        _ -> fail $ printf "Invalid BSON binary subtype: %i" tag
{-# INLINE getBsonBinary #-}

putBsonObjectId :: BsonObjectId -> Put
putBsonObjectId BsonObjectId { .. } = do
    putWord32le bsonObjectIdTime
    putWord32le bsonObjectIdMachine
    putWord16le bsonObjectIdPid
    putWord24le bsonObjectIdInc
{-# INLINE putBsonObjectId #-}

getBsonObjectId :: Get BsonObjectId
getBsonObjectId = BsonObjectId <$> getWord32le
                               <*> getWord32le
                               <*> getWord16le
                               <*> getWord24le
{-# INLINE getBsonObjectId #-}

putBsonUtcTime :: UTCTime -> Put
putBsonUtcTime time = putWord64le $ round $ utcTimeToPOSIXSeconds time * 1000
{-# INLINE putBsonUtcTime #-}

getBsonUtcTime :: Get UTCTime
getBsonUtcTime = do
    time <- fromIntegral <$> getWord64le
    return $ posixSecondsToUTCTime $ fromInteger $ time `div` 1000
{-# INLINE getBsonUtcTime #-}

putBsonJsWithScope :: BsonDocument -> Text -> Put
putBsonJsWithScope doc code = do
    putWord32le $ fromIntegral $ L.length bytes + 4
    putLazyByteString bytes
  where
      bytes = runPut $ do
          putBsonString code
          putBsonDocument doc
{-# INLINE putBsonJsWithScope #-}

getBsonJsWithScope :: Get (BsonDocument, Text)
getBsonJsWithScope = do
    len <- fromIntegral <$> getWord32le
    bytes <- getLazyByteString $ len - 4
    return $ flip runGet bytes $ do
        code <- getBsonString
        document <- getBsonDocument
        return (document, code)
{-# INLINE getBsonJsWithScope #-}

putBsonCString :: Text -> Put
putBsonCString s = do
    putByteString $ TE.encodeUtf8 s
    putWord8 0x00
{-# INLINE putBsonCString #-}

getBsonCString :: Get Text
getBsonCString =
    TE.decodeUtf8 . S.concat . L.toChunks <$> getLazyByteStringNul
{-# INLINE getBsonCString #-}

putBsonString :: Text -> Put
putBsonString s = let bytes = TE.encodeUtf8 s in do
    putWord32le $ fromIntegral (S.length bytes + 1)
    putByteString bytes
    putWord8 0x00
{-# INLINE putBsonString #-}

getBsonString :: Get Text
getBsonString = do
    len <- fromIntegral <$> getWord32le
    payload <- getByteString $ len - 1
    getWord8 >>= \z -> case z of
        0x00 -> return $ TE.decodeUtf8 payload
        _    -> fail "BSON string is not NULL terminated"
{-# INLINE getBsonString #-}

getBsonBool :: Get Bool
getBsonBool = getWord8 >>= \v -> case v of
    0x00 -> return False
    0x01 -> return True
    _    -> fail $ printf "Invalind BSON boolean: " v
{-# INLINE getBsonBool #-}

putWord24le :: Word24 -> Put
putWord24le w = do
    putWord8 $ fromIntegral w
    putWord8 $ fromIntegral $ shiftR w 8
    putWord8 $ fromIntegral $ shiftR w 16
{-# INLINE putWord24le #-}

getWord24le :: Get Word24
getWord24le = do
    b1 <- fromIntegral <$> getWord8
    b2 <- fromIntegral <$> getWord8
    b3 <- fromIntegral <$> getWord8
    return $ fromInteger $ shiftL b3 16 .|. shiftL b2 8 .|. b1
{-# INLINE getWord24le #-}
