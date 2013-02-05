module Main where

import Test.Framework (defaultMain)

import qualified Data.Bson.Instances.Tests
import qualified Data.Bson.Binary.Tests

main :: IO ()
main = defaultMain
    [ Data.Bson.Instances.Tests.tests
    , Data.Bson.Binary.Tests.tests
    ]
