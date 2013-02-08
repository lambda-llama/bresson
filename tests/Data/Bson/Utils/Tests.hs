{-# LANGUAGE OverloadedStrings #-}

module Data.Bson.Utils.Tests
    ( tests
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, (==>))
import qualified Data.Text as ST

import Data.Bson (BsonDocument, BsonLabel, BsonValue(BsonValueDocument),
                  (=:), (!?), document)
import Data.Bson.Tests.Instances ()

testRecursiveLookup :: [BsonLabel] -> BsonValue -> Property
testRecursiveLookup labels v =
    (length labels > 0 && all isProperLabel labels) ==>
    doc !? (ST.intercalate "." labels) == Just v
  where
    isProperLabel = not . ST.any (== '.')

    doc :: BsonDocument
    BsonValueDocument doc =
        foldr (\l acc -> BsonValueDocument $ document [l =: acc]) v labels

tests :: Test
tests = testGroup "Data.Bson.Utils.Tests"
    [ testProperty "testRecursiveLookup" testRecursiveLookup
    ]
