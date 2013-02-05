module Main where

import System.Random (StdGen, getStdGen)

import Criterion.Main (defaultMain, bench, whnf)
import Data.Text (Text)
import Test.QuickCheck (Arbitrary, arbitrary, resize)
import Test.QuickCheck.Gen (unGen)

import Data.Bson.Class (FromBson(..), ToBson(..))
import Data.Bson.Instances ()
import Data.Bson.Tests.Instances ()
import Data.Bson.Types (BsonValue, BsonDocument)

main :: IO ()
main = do
    stdGen <- getStdGen
    defaultMain
        [ bench "Double" (whnf f (generate stdGen :: Double))
        , bench "Text" (whnf f (generate stdGen :: Text))
        , bench "Document" (whnf f (generate stdGen :: BsonDocument))
        ]
  where
    n :: Int
    n = 2 ^ (12 :: Int)

    seed :: Int
    seed = 42

    f :: (FromBson a, ToBson a) => a -> a
    f x = case fromBson $ toBson x of
        Left _  -> error "fromBson"
        Right x -> x

    generate  :: (FromBson a, ToBson a, Arbitrary a) => StdGen -> a
    generate stdGen = unGen (resize n arbitrary) stdGen seed
