{-# LANGUAGE OverloadedStrings #-}

module Data.Bson.Utils.Tests
    ( tests
    ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary(..), NonEmptyList(..), suchThat, testProperty)
import qualified Data.Text as ST

import Data.Bson (Document, Label, Value(ValueDocument),
                  (=:), (!?), document, parseMaybe)
import Data.Bson.Tests.Instances ()

newtype ProperLabel = ProperLabel { unProperLabel :: Label }
    deriving (Show)

instance Arbitrary ProperLabel where
    arbitrary = fmap ProperLabel $ arbitrary `suchThat` (not . ST.any (== '.'))

testRecursiveLookup :: NonEmptyList ProperLabel -> Value -> Bool
testRecursiveLookup properLabels v = parseMaybe parser doc == Just v
  where
    labels = map unProperLabel $ getNonEmpty properLabels
    parser d = d !? (ST.intercalate "." labels)
    doc :: Document
    ValueDocument doc =
        foldr (\l acc -> ValueDocument $ document [l =: acc]) v labels

tests :: TestTree
tests = testGroup "Data.Bson.Utils.Tests"
    [ testProperty "testRecursiveLookup" testRecursiveLookup
    ]
