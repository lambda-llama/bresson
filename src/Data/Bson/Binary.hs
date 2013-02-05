{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Data.Bson.Binary () where

import Prelude hiding (length, concat)
import Control.Applicative ((<$>), (<*>))
import Data.Binary (Binary(..))
import Data.Binary.Get (Get, runGet, getWord8, getWord16le,
                        getWord32le, getWord64le, getLazyByteStringNul,
                        getLazyByteString, getByteString, lookAhead)
import Data.Binary.Put (Put, runPut, putWord8, putWord16le, putWord32le,
                        putWord64le, putLazyByteString, putByteString)
import Data.Binary.IEEE754 (getFloat64le, putFloat64le)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

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
        BsonValueDouble a -> (0x01, putFloat64le a)
        BsonValueString a -> (0x02, putBsonString a)
        BsonValueDocument a -> (0x03, putBsonDocument a)
        BsonValueArray a -> (0x04, putBsonArray a)
        BsonValueBinary a -> (0x05, putBsonBinary a)
        BsonValueObjectId a -> (0x07, putBsonObjectId a)
        BsonValueBool False -> (0x08, putWord8 0)
        BsonValueBool True -> (0x08, putWord8 1)
        BsonValueUtcTime a -> (0x09, putBsonUtcTime a)
        BsonValueNull -> (0x0a, return ())
        BsonValueRegex a b -> (0x0b, putBsonCString a >> putBsonCString b)
        BsonValueJavascript a -> (0x0d, putBsonCString a)
        BsonValueJavascriptWithScope a b -> (0x0f, putBsonJsWithScope a b)
        BsonValueInt32 a -> (0x10, putWord32le $ fromIntegral a)
        BsonValueInt64 a -> (0x11, putWord64le $ fromIntegral a)
        BsonValueTimestamp a -> (0x12, putWord64le $ fromIntegral a)
        BsonValueMin -> (0xff, return ())
        BsonValueMax -> (0x7f, return ())
{-# INLINE putBsonField #-}

getBsonField :: Get (BsonLabel, BsonValue)
getBsonField = do
    tag <- getWord8
    key <- getBsonCString
    value <- case tag of
        0x01 -> BsonValueDouble <$> getFloat64le
        0x02 -> BsonValueString <$> getBsonString
        0x03 -> BsonValueDocument <$> getBsonDocument
        0x04 -> BsonValueArray <$> getBsonArray
        0x05 -> BsonValueBinary <$> getBsonBinary
        0x07 -> BsonValueObjectId <$> getBsonObjectId
        0x08 -> BsonValueBool <$> getBsonBool
        0x09 -> BsonValueUtcTime <$> getBsonUtcTime
        0x0a -> return BsonValueNull
        0x0b -> BsonValueRegex <$> getBsonCString <*> getBsonCString
        0x0d -> BsonValueJavascript <$> getBsonCString
        0x0f -> uncurry BsonValueJavascriptWithScope <$> getBsonJsWithScope
        0x10 -> BsonValueInt32 . fromIntegral <$> getWord32le
        0x11 -> BsonValueInt64 . fromIntegral <$> getWord64le
        0x12 -> BsonValueTimestamp . fromIntegral <$> getWord64le
        0xff -> return BsonValueMin
        0x7f -> return BsonValueMax
        _    -> fail "Invalid bson value"
    return (key, value)
{-# INLINE getBsonField #-}

putBsonDocument :: BsonDocument -> Put
putBsonDocument doc = do
    -- include length and null terminator
    putWord32le $ fromIntegral $ L.length bytes + 5
    putLazyByteString bytes
    putWord8 0x00
  where
    bytes = runPut $ sequence_ $ HashMap.foldlWithKey' f [] doc
    f a k v = (putBsonField k v) : a
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

putBsonArray :: BsonArray -> Put
putBsonArray array = putBsonDocument doc
  where
    inf = Vector.generate (Vector.length array) $ intToText
    doc = Vector.foldl' f HashMap.empty $ Vector.zip inf array
    f a (k, v) = HashMap.insert k v a
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
        BsonBinaryGeneric a     -> (0x00, a)
        BsonBinaryFunction a    -> (0x01, a)
        BsonBinaryUuid a         -> (0x04, a)
        BsonBinaryMd5 a         -> (0x05, a)
        BsonBinaryUserDefined a -> (0x80, a)
{-# INLINE putBsonBinary #-}

getBsonBinary :: Get BsonBinary
getBsonBinary = do
    len <- fromIntegral <$> getWord32le
    tag <- getWord8
    bytes <- getByteString len
    case tag of
        0x00 -> return $ BsonBinaryGeneric bytes
        0x01 -> return $ BsonBinaryFunction bytes
        0x04 -> return $ BsonBinaryUuid bytes
        0x05 -> return $ BsonBinaryMd5 bytes
        0x80 -> return $ BsonBinaryUserDefined bytes
        _     -> fail "Invalid bson binary"
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
        _     -> fail "Invalid bson string"
{-# INLINE getBsonString #-}

getBsonBool :: Get Bool
getBsonBool = getWord8 >>= \t -> case t of
    0x00 -> return False
    0x01 -> return True
    _     -> fail "Invalind bson bool"
{-# INLINE getBsonBool #-}

putWord24le :: Word24 -> Put
putWord24le w = do
    putWord8 $ fromIntegral $ w
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
