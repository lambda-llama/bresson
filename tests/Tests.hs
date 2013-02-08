module Main where

import Test.Framework (defaultMain)

import qualified Data.Bson.Binary.Tests
import qualified Data.Bson.Instances.Tests
import qualified Data.Bson.Utils.Tests

main :: IO ()
main = defaultMain
    [ Data.Bson.Binary.Tests.tests
    , Data.Bson.Instances.Tests.tests
    , Data.Bson.Utils.Tests.tests
    ]
