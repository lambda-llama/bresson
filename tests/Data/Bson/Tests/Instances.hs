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

import Data.Bson (Document, Value(..), Binary(..),
                  ObjectId(..), Array, RegexOption(..),
                  RegexOptions)

instance Arbitrary Array where
    arbitrary = fmap Vector.fromList arbitrary

instance Arbitrary Document where
    arbitrary = fmap HashMap.fromList arbitrary

instance Arbitrary S.ByteString where
    arbitrary = S.pack <$> arbitrary

instance Arbitrary Text where
    arbitrary = T.filter ((/= '\NUL')) . T.pack <$> arbitrary

instance Arbitrary UTCTime where
    arbitrary = do
        Positive a <- arbitrary
        return $ posixSecondsToUTCTime $ fromInteger a

instance Arbitrary Value where
    arbitrary = oneof
        [ ValueDouble <$> arbitrary
        , ValueString <$> arbitrary
        , resize 5 $ ValueDocument <$> arbitrary
        , resize 5 $ ValueArray <$> arbitrary
        , ValueBinary <$> arbitrary
        , ValueObjectId <$> arbitrary
        , ValueBool <$> arbitrary
        , ValueUtcTime <$> arbitrary
        , return ValueNull
        , ValueRegex <$> arbitrary <*> arbitrary
        , ValueJavascript <$> arbitrary
        , resize 5 $ ValueJavascriptWithScope <$> arbitrary <*> arbitrary
        , ValueInt32 <$> arbitrary
        , ValueInt64 <$> arbitrary
        , ValueTimestamp <$> arbitrary
        , return ValueMin
        , return ValueMax
        ]

instance Arbitrary Binary where
    arbitrary = oneof
        [ BinaryGeneric <$> arbitrary
        , BinaryFunction <$> arbitrary
        , BinaryUuid <$> arbitrary
        , BinaryMd5 <$> arbitrary
        , BinaryUserDefined <$> arbitrary
        ]

instance Arbitrary ObjectId where
    arbitrary = ObjectId <$> arbitrary
                             <*> arbitrary
                             <*> arbitrary
                             <*> arbitrary

instance Arbitrary RegexOption where
    arbitrary = elements [ RegexOptionCaseInsensitive
                         , RegexOptionLocaleDependent
                         , RegexOptionMultiline
                         , RegexOptionDotall
                         , RegexOptionUnicode
                         , RegexOptionVerbose
                         ]

instance Arbitrary RegexOptions where
    arbitrary = BitSet.fromList <$> arbitrary

instance Arbitrary Word24 where
    arbitrary = fromInteger <$> arbitrary

instance Arbitrary UUID where
    arbitrary = fromWords <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
