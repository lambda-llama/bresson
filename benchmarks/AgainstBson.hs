{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

module Main where

import Control.DeepSeq (NFData(..), force)
import Control.Monad (replicateM)
import Data.ByteString.Lazy (ByteString)
import Data.Binary (decode, encode)
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import System.Random (StdGen, newStdGen)
#if !MIN_VERSION_bytestring(0,10,0)
import qualified Data.ByteString.Lazy as L
#endif

import Criterion.Main (defaultMain, bench, nf)
import Control.DeepSeq.TH (deriveNFDatas)
import Test.QuickCheck (arbitrary, resize)
import Test.QuickCheck.Gen (unGen)
import "bresson" Data.Bson.Binary ()
import "bresson" Data.Bson.Tests.Instances ()
import qualified "bresson" Data.Bson as Bresson
import qualified "bson" Data.Bson as Bson
import qualified "bson" Data.Bson.Binary as Bson

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData ByteString where
    rnf x = L.length x `seq` ()
#endif

deriving instance NFData Bson.Binary
deriving instance NFData Bson.Function
deriving instance NFData Bson.UUID
deriving instance NFData Bson.MD5
deriving instance NFData Bson.UserDefined
deriving instance NFData Bson.Symbol
deriving instance NFData Bson.MongoStamp

$(deriveNFDatas [''Bson.Value, ''Bson.MinMaxKey, ''Bson.Regex,
                 ''Bson.Javascript, ''Bson.ObjectId, ''Bson.Field])

encodeBresson :: [Bresson.BsonDocument] -> [ByteString]
encodeBresson = map encode

decodeBresson :: [ByteString] -> [Bresson.BsonDocument]
decodeBresson = map decode

encodeBson :: [Bson.Document] -> [ByteString]
encodeBson = map (runPut . Bson.putDocument)

decodeBson :: [ByteString] -> [Bson.Document]
decodeBson = map (runGet Bson.getDocument)

generate :: Int -> Int -> IO ByteString
generate size seed = newStdGen >>= \gen ->
    return $ encode $ mkDoc gen
  where
    mkDoc :: StdGen -> Bresson.BsonDocument
    mkDoc gen = unGen (resize size arbitrary) gen seed

main :: IO ()
main = do
    testData <- replicateM 100 $ generate 100 42
    let bson = force $ decodeBson testData
    let bresson = force $ decodeBresson testData
    defaultMain [ bench "decode bson" $ nf decodeBson testData
                , bench "decode bresson" $ nf decodeBresson testData
                , bench "encode bson" $ nf encodeBson bson
                , bench "encode bresson" $ nf encodeBresson bresson
                ]
