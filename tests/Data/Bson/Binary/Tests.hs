module Data.Bson.Binary.Tests
    ( tests
    ) where

import Data.Binary (encode, decode)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Data.Bson (BsonDocument)
import Data.Bson.Binary ()
import Data.Bson.Tests.Instances ()

testEncodeDecodeDocument :: BsonDocument -> Bool
testEncodeDecodeDocument doc = (==) doc $ decode $ encode doc

tests :: TestTree
tests = testGroup "Data.Bson.Binary.Tests"
    [ testProperty "testEncodeDecodeDocument" testEncodeDecodeDocument
    ]
