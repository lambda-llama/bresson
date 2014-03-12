module Main where

import Data.Int (Int32, Int64)
import Data.Time.Clock (UTCTime)
import System.Random (StdGen, getStdGen)

import Criterion.Main (defaultMain, bench, bgroup, whnf)
import Data.Text (Text)
import Test.QuickCheck (Arbitrary, arbitrary, resize)
import Test.QuickCheck.Gen (unGen)

import Data.Bson (FromBson(..), ToBson(..), Document, parseMaybe)
import Data.Bson.Tests.Instances ()

main :: IO ()
main = do
    stdGen <- getStdGen
    defaultMain
        [ bgroup "fromBson . toBson"
          [ bench "Double" (whnf f (generate stdGen :: Double))
          , bench "Text" (whnf f (generate stdGen :: Text))
          , bench "Document" (whnf f (generate stdGen :: Document))
          , bench "Bool" (whnf f (generate stdGen :: Bool))
          , bench "UTCTime" (whnf f (generate stdGen :: UTCTime))
          , bench "Int32" (whnf f (generate stdGen :: Int32))
          , bench "Int64" (whnf f (generate stdGen :: Int64))
          ]
        ]
  where
    n :: Int
    n = 2 ^ (12 :: Int)

    seed :: Int
    seed = 42

    f :: (FromBson a, ToBson a) => a -> a
    f x = case parseMaybe fromBson $ toBson x of
        Nothing  -> error "fromBson"
        Just a -> a

    generate  :: (FromBson a, ToBson a, Arbitrary a) => StdGen -> a
    generate stdGen = unGen (resize n arbitrary) stdGen seed
