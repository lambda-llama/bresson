{-# LANGUAGE OverloadedStrings #-}

module Data.Bson.Utils.Tests
    ( tests
    ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
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

tests :: TestTree
tests = testGroup "Data.Bson.Utils.Tests"
    [ testProperty "testRecursiveLookup" testRecursiveLookup
    ]
