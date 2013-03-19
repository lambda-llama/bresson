{-# LANGUAGE PackageImports #-}

module Main where

import Control.Monad (replicateM)
import Data.ByteString.Lazy (ByteString)
import Data.Binary (decode, encode)
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import System.Random (StdGen, newStdGen)

import Criterion.Main (defaultMain, bench, nf)
import Test.QuickCheck (arbitrary, resize)
import Test.QuickCheck.Gen (unGen)
import "bresson" Data.Bson.Binary ()
import "bresson" Data.Bson.Tests.Instances ()
import qualified "bresson" Data.Bson as Bresson
import qualified "bson" Data.Bson.Binary as Bson

testBresson :: [ByteString] -> [ByteString]
testBresson = map (encode . (decode :: ByteString -> Bresson.BsonDocument))

testBson :: [ByteString] -> [ByteString]
testBson = map (runPut . Bson.putDocument . runGet Bson.getDocument)

generate :: Int -> Int -> IO ByteString
generate size seed = newStdGen >>= \gen ->
    return $ encode $ mkDoc gen
  where
    mkDoc :: StdGen -> Bresson.BsonDocument
    mkDoc gen = unGen (resize size arbitrary) gen seed

main :: IO ()
main = do
    testData <- replicateM 100 $ generate 100 42
    defaultMain [ bench "bson" $ nf testBson testData
                , bench "bresson" $ nf testBresson testData
                ]
