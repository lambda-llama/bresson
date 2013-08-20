module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Data.Bson.Binary.Tests
import qualified Data.Bson.Instances.Tests
import qualified Data.Bson.Utils.Tests

main :: IO ()
main = defaultMain . testGroup "Tests" $
    [ Data.Bson.Binary.Tests.tests
    , Data.Bson.Instances.Tests.tests
    , Data.Bson.Utils.Tests.tests
    ]
