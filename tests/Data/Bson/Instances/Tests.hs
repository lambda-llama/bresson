module Data.Bson.Instances.Tests
    ( tests
    ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Time.Clock (UTCTime)
import qualified Data.ByteString as S

import Data.Text (Text)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Bson (BsonValue, ToBson(..), FromBson(..))
import Data.Bson.Tests.Instances ()

testToFromBson :: (ToBson a, FromBson a, Eq a) => a -> Bool
testToFromBson a = case fromBson . toBson $ a of
    Left _ -> False
    Right a' -> a == a'

tests :: Test
tests = testGroup "Data.Bson.Instances.Tests"
    [ testProperty "BsonValue" (testToFromBson :: BsonValue -> Bool)
    , testProperty "Text" (testToFromBson :: Text -> Bool)
    -- , testProperty "String" (testToFromBson :: String -> Bool)
    , testProperty "Strict ByteString" (testToFromBson :: S.ByteString -> Bool)
    , testProperty "Bool" (testToFromBson :: Bool -> Bool)
    , testProperty "Int8" (testToFromBson :: Int8 -> Bool)
    , testProperty "Int16" (testToFromBson :: Int16 -> Bool)
    , testProperty "Int32" (testToFromBson :: Int32 -> Bool)
    , testProperty "Int64" (testToFromBson :: Int64 -> Bool)
    , testProperty "Int" (testToFromBson :: Int -> Bool)
    , testProperty "Double" (testToFromBson :: Double -> Bool)
    , testProperty "UTCTime" (testToFromBson :: UTCTime -> Bool)
    ]
