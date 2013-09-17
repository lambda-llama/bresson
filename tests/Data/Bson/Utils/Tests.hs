{-# LANGUAGE OverloadedStrings #-}

module Data.Bson.Utils.Tests
    ( tests
    ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Property, (==>))
import qualified Data.Text as ST

import Data.Bson (Document, Label, Value(ValueDocument),
                  (=:), (!?), document)
import Data.Bson.Tests.Instances ()

testRecursiveLookup :: [Label] -> Value -> Property
testRecursiveLookup labels v =
    (length labels > 0 && all isProperLabel labels) ==>
    doc !? (ST.intercalate "." labels) == Just v
  where
    isProperLabel = not . ST.any (== '.')

    doc :: Document
    ValueDocument doc =
        foldr (\l acc -> ValueDocument $ document [l =: acc]) v labels

tests :: TestTree
tests = testGroup "Data.Bson.Utils.Tests"
    [ testProperty "testRecursiveLookup" testRecursiveLookup
    ]
