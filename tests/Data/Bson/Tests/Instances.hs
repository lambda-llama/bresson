{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Bson.Tests.Instances () where

import Control.Applicative ((<$>), (<*>))
import Data.Time.Clock (UTCTime(..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.ByteString as S

import Data.Text (Text)
import Data.Word.Word24 (Word24)
import Data.UUID (UUID, fromWords)
import Test.QuickCheck (Positive(..), Arbitrary(..), oneof, resize, elements)
import qualified Data.BitSet.Word as BitSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Vector as Vector

import Data.Bson (BsonDocument, BsonValue(..), BsonBinary(..),
                  BsonObjectId(..), BsonArray, BsonRegexOption(..),
                  BsonRegexOptions)

instance Arbitrary BsonArray where
    arbitrary = fmap Vector.fromList arbitrary

instance Arbitrary BsonDocument where
    arbitrary = fmap HashMap.fromList arbitrary

instance Arbitrary S.ByteString where
    arbitrary = S.pack <$> arbitrary

instance Arbitrary Text where
    arbitrary = T.filter ((/= '\NUL')) . T.pack <$> arbitrary

instance Arbitrary UTCTime where
    arbitrary = do
        Positive a <- arbitrary
        return $ posixSecondsToUTCTime $ fromInteger a

instance Arbitrary BsonValue where
    arbitrary = oneof
        [ BsonValueDouble <$> arbitrary
        , BsonValueString <$> arbitrary
        , resize 5 $ BsonValueDocument <$> arbitrary
        , resize 5 $ BsonValueArray <$> arbitrary
        , BsonValueBinary <$> arbitrary
        , BsonValueObjectId <$> arbitrary
        , BsonValueBool <$> arbitrary
        , BsonValueUtcTime <$> arbitrary
        , return BsonValueNull
        , BsonValueRegex <$> arbitrary <*> arbitrary
        , BsonValueJavascript <$> arbitrary
        , resize 5 $ BsonValueJavascriptWithScope <$> arbitrary <*> arbitrary
        , BsonValueInt32 <$> arbitrary
        , BsonValueInt64 <$> arbitrary
        , BsonValueTimestamp <$> arbitrary
        , return BsonValueMin
        , return BsonValueMax
        ]

instance Arbitrary BsonBinary where
    arbitrary = oneof
        [ BsonBinaryGeneric <$> arbitrary
        , BsonBinaryFunction <$> arbitrary
        , BsonBinaryUuid <$> arbitrary
        , BsonBinaryMd5 <$> arbitrary
        , BsonBinaryUserDefined <$> arbitrary
        ]

instance Arbitrary BsonObjectId where
    arbitrary = BsonObjectId <$> arbitrary
                             <*> arbitrary
                             <*> arbitrary
                             <*> arbitrary

instance Arbitrary BsonRegexOption where
    arbitrary = elements [ BsonRegexOptionCaseInsensitive
                         , BsonRegexOptionLocaleDependent
                         , BsonRegexOptionMultiline
                         , BsonRegexOptionDotall
                         , BsonRegexOptionUnicode
                         , BsonRegexOptionVerbose
                         ]

instance Arbitrary BsonRegexOptions where
    arbitrary = BitSet.fromList <$> arbitrary

instance Arbitrary Word24 where
    arbitrary = fromInteger <$> arbitrary

instance Arbitrary UUID where
    arbitrary = fromWords <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
