{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Data.Bson.Binary
    ( putField
    , getField
    , putDocument
    , getDocument
    , putArray
    , getArray
    , putBinary
    , getBinary
    , putObjectId
    , getObjectId
    , putUtcTime
    , getUtcTime
    , putRegexOptions
    , getRegexOptions
    , putJsWithScope
    , getJsWithScope
    , putCString
    , getCString
    , putString
    , getString
    ) where

#include "bson.h"

import Prelude hiding (length, concat)
import Control.Applicative (pure, (<$>), (<*>), (*>))
import Data.Binary (get, put)
import Data.Binary.Get (Get, runGet, getWord8, getWord16le,
                        getWord32le, getWord64le, getLazyByteStringNul,
                        getLazyByteString, getByteString, lookAhead)
import Data.Binary.Put (Put, runPut, putWord8, putWord16le, putWord32le,
                        putWord64le, putLazyByteString, putByteString)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Text.Printf (printf)
import qualified Data.Binary as Binary

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Char8 as L

import Data.Binary.IEEE754 (getFloat64le, putFloat64le)
import Data.Text (Text)
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Word.Word24 (Word24)
import qualified Data.BitSet.Word as BitSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as Vector
import qualified Data.UUID as UUID

import Data.Bson.Types (Document, Array, Label, Value(..),
                        Binary(..), ObjectId(..),
                        RegexOption(..), RegexOptions)

instance Binary.Binary Document where
    put = putDocument
    get = getDocument

putField :: Label -> Value -> Put
putField key value = do
    putWord8 tag
    putCString key
    payload
  where
    (tag, payload) = case value of
        ValueDouble a     -> (BSON_DOUBLE, putFloat64le a)
        ValueString a     -> (BSON_STRING, putString a)
        ValueDocument a   -> (BSON_DOCUMENT, putDocument a)
        ValueArray a      -> (BSON_ARRAY, putArray a)
        ValueBinary a     -> (BSON_BINARY, putBinary a)
        ValueObjectId a   -> (BSON_OID, putObjectId a)
        ValueBool False   -> (BSON_BOOLEAN, putWord8 0)
        ValueBool True    -> (BSON_BOOLEAN, putWord8 1)
        ValueUtcTime a    -> (BSON_UTC, putUtcTime a)
        ValueNull         -> (BSON_NULL, return ())
        ValueRegex a b    ->
            (BSON_REGEX, putCString a >> putRegexOptions b)
        ValueJavascript a -> (BSON_JS, putString a)
        ValueJavascriptWithScope a b ->
            (BSON_JS_WITH_SCOPE, putJsWithScope a b)
        ValueInt32 a      -> (BSON_INT32, putWord32le $ fromIntegral a)
        ValueInt64 a      -> (BSON_INT64, putWord64le $ fromIntegral a)
        ValueTimestamp a  -> (BSON_TIMESTAMP, putWord64le $ fromIntegral a)
        ValueMin          -> (BSON_MIN, return ())
        ValueMax          -> (BSON_MAX, return ())
{-# INLINE putField #-}

getField :: Get (Label, Value)
getField = do
    tag <- getWord8
    key <- getCString
    value <- case tag of
        BSON_DOUBLE    -> ValueDouble <$> getFloat64le
        BSON_STRING    -> ValueString <$> getString
        BSON_DOCUMENT  -> ValueDocument <$> getDocument
        BSON_ARRAY     -> ValueArray <$> getArray
        BSON_BINARY    -> ValueBinary <$> getBinary
        BSON_OID       -> ValueObjectId <$> getObjectId
        BSON_BOOLEAN   -> ValueBool <$> getBool
        BSON_UTC       -> ValueUtcTime <$> getUtcTime
        BSON_NULL      -> return ValueNull
        BSON_REGEX     -> ValueRegex <$> getCString <*> getRegexOptions
        BSON_JS        -> ValueJavascript <$> getString
        BSON_JS_WITH_SCOPE -> uncurry ValueJavascriptWithScope <$> getJsWithScope
        BSON_INT32     -> ValueInt32 . fromIntegral <$> getWord32le
        BSON_INT64     -> ValueInt64 . fromIntegral <$> getWord64le
        BSON_TIMESTAMP -> ValueTimestamp . fromIntegral <$> getWord64le
        BSON_MIN       -> return ValueMin
        BSON_MAX       -> return ValueMax
        _              -> fail $ printf "Invalid BSON tag: %i" tag
    return (key, value)
{-# INLINE getField #-}

putAsDocument :: Put -> Put
putAsDocument p = do
    -- include length and null terminator
    putWord32le $ fromIntegral $ L.length bytes + 5
    putLazyByteString bytes
    putWord8 0x00
  where
    bytes = runPut p
{-# INLINE putAsDocument #-}

putDocument :: Document -> Put
putDocument doc = putAsDocument $ HashMap.foldlWithKey' f (return ()) doc
  where
    f a k v = a *> putField k v
{-# INLINE putDocument #-}

getDocument :: Get Document
getDocument = do
    l <- fromIntegral <$> getWord32le
    bytes <- getLazyByteString $ l - 4
    return $ HashMap.fromList $ runGet getFields bytes
 where
    getFields = lookAhead getWord8 >>= \done -> if done == 0
        then return []
        else (:) <$> getField
                 <*> getFields
{-# INLINE getDocument #-}

putArray :: Array -> Put
putArray array = putAsDocument $ Vector.zipWithM_ putField inf array
  where
    inf = Vector.generate (Vector.length array) intToText
{-# INLINE putArray #-}

getArray :: Get Array
getArray = getDocument >>= \doc ->
    return $ Vector.unfoldr (f doc (HashMap.size doc)) 0
  where
    f doc s c | c >= s    = Nothing
              | otherwise = HashMap.lookup (intToText c) doc >>=
                     Just . (, c + 1)
{-# INLINE getArray #-}

putBinary :: Binary -> Put
putBinary binary = do
    putWord32le $ fromIntegral $ S.length payload
    putWord8 tag
    putByteString payload
  where
    (tag, payload) = case binary of
        BinaryGeneric a     -> (BSON_GENERIC_BINARY, a)
        BinaryFunction a    -> (BSON_FUNCTION, a)
        BinaryUuid a        -> (BSON_UUID, lazyByteStringToStrict $ UUID.toByteString a)
        BinaryMd5 a         -> (BSON_MD5, a)
        BinaryUserDefined a -> (BSON_USER_DEFINED_BINARY, a)
{-# INLINE putBinary #-}

getBinary :: Get Binary
getBinary = do
    len <- fromIntegral <$> getWord32le
    tag <- getWord8
    bytes <- getByteString len
    case tag of
        BSON_GENERIC_BINARY -> return $ BinaryGeneric bytes
        BSON_FUNCTION -> return $ BinaryFunction bytes
        BSON_BINARY_OLD -> return $ BinaryGeneric bytes
        BSON_UUID_OLD -> mkUuid bytes
        BSON_UUID -> mkUuid bytes
        BSON_MD5 -> return $ BinaryMd5 bytes
        BSON_USER_DEFINED_BINARY -> return $ BinaryUserDefined bytes
        _ -> fail $ printf "Invalid BSON binary subtype: %i" tag
  where
    mkUuid bytes = case UUID.fromByteString $ strictByteStringToLazy bytes of
        Just uuid -> return $ BinaryUuid uuid
        Nothing   -> fail "Invalid BSON binary uuid"
{-# INLINE getBinary #-}

putObjectId :: ObjectId -> Put
putObjectId ObjectId { .. } = do
    putWord32le objectIdTime
    putWord24le objectIdMachine
    putWord16le objectIdPid
    putWord24le objectIdInc
{-# INLINE putObjectId #-}

getObjectId :: Get ObjectId
getObjectId = ObjectId <$> getWord32le
                       <*> getWord24le
                       <*> getWord16le
                       <*> getWord24le
{-# INLINE getObjectId #-}

putUtcTime :: UTCTime -> Put
putUtcTime time = putWord64le $ round $ utcTimeToPOSIXSeconds time * 1000
{-# INLINE putUtcTime #-}

getUtcTime :: Get UTCTime
getUtcTime = do
    time <- fromIntegral <$> getWord64le
    return $ posixSecondsToUTCTime $ fromInteger $ time `div` 1000
{-# INLINE getUtcTime #-}

putRegexOptions :: RegexOptions -> Put
putRegexOptions opts = BitSet.foldl' f (pure ()) opts *> putWord8 0x00
  where
    f a v = (put $ match v) *> a
    match RegexOptionCaseInsensitive = 'i'
    match RegexOptionLocaleDependent = 'l'
    match RegexOptionMultiline       = 'm'
    match RegexOptionDotall          = 's'
    match RegexOptionUnicode         = 'u'
    match RegexOptionVerbose         = 'x'
{-# INLINE putRegexOptions #-}

getRegexOptions :: Get RegexOptions
getRegexOptions = fmap (L.foldl' f BitSet.empty) getLazyByteStringNul
  where
    f c = flip BitSet.insert c . match
    match 'i' = RegexOptionCaseInsensitive
    match 'l' = RegexOptionLocaleDependent
    match 'm' = RegexOptionMultiline
    match 's' = RegexOptionDotall
    match 'u' = RegexOptionUnicode
    match 'x' = RegexOptionVerbose
    match v   = error $ printf "Invalind BSON regex option: %c" v
{-# INLINE getRegexOptions #-}

putJsWithScope :: Text -> Document -> Put
putJsWithScope code document = do
    putWord32le $ fromIntegral $ L.length bytes + 4
    putLazyByteString bytes
  where
      bytes = runPut $ do
          putString code
          putDocument document
{-# INLINE putJsWithScope #-}

getJsWithScope :: Get (Text, Document)
getJsWithScope = do
    len <- fromIntegral <$> getWord32le
    bytes <- getLazyByteString $ len - 4
    return $ flip runGet bytes $ do
        code <- getString
        document <- getDocument
        return (code, document)
{-# INLINE getJsWithScope #-}

putCString :: Text -> Put
putCString s = do
    putByteString $ TE.encodeUtf8 s
    putWord8 0x00
{-# INLINE putCString #-}

getCString :: Get Text
getCString = TE.decodeUtf8 . S.concat . L.toChunks <$> getLazyByteStringNul
{-# INLINE getCString #-}

putString :: Text -> Put
putString s = let bytes = TE.encodeUtf8 s in do
    putWord32le $ fromIntegral (S.length bytes + 1)
    putByteString bytes
    putWord8 0x00
{-# INLINE putString #-}

getString :: Get Text
getString = do
    len <- fromIntegral <$> getWord32le
    payload <- getByteString $ len - 1
    getWord8 >>= \z -> case z of
        0x00 -> return $ TE.decodeUtf8 payload
        _    -> fail "BSON string is not NULL terminated"
{-# INLINE getString #-}

getBool :: Get Bool
getBool = getWord8 >>= \v -> case v of
    0x00 -> return False
    0x01 -> return True
    _    -> fail $ printf "Invalind BSON boolean: %i" v
{-# INLINE getBool #-}

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

intToText :: Int -> Text
intToText = TL.toStrict . TL.toLazyText . decimal
{-# INLINE intToText #-}

lazyByteStringToStrict :: L.ByteString -> S.ByteString
strictByteStringToLazy :: S.ByteString -> L.ByteString
#if MIN_VERSION_bytestring(0, 10, 0)
lazyByteStringToStrict = L.toStrict
strictByteStringToLazy = L.fromStrict
#else
lazyByteStringToStrict = S.concat . L.toChunks
strictByteStringToLazy = L.fromChunks . return
#endif
{-# INLINE lazyByteStringToStrict #-}
{-# INLINE strictByteStringToLazy #-}
