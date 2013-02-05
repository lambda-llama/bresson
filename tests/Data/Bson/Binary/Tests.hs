module Data.Bson.Binary.Tests
    ( tests
    ) where

import Data.Binary (encode, decode)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Bson.Types (BsonDocument)
import Data.Bson.Binary ()
import Data.Bson.Tests.Instances ()
import Debug.Trace

testEncodeDecodeDocument :: BsonDocument -> Bool 
testEncodeDecodeDocument doc = (==) doc $ ((trace $ show decoded) $ decode $ encode doc)
  where
    decoded :: BsonDocument
    decoded = decode $ encode doc

tests :: Test
tests = testGroup "Data.Bson.Binary.Tests" 
    [ testProperty "testEncodeDecodeDocument" testEncodeDecodeDocument
    ]
