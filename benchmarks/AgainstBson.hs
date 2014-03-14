{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import GHC.Generics

import Control.DeepSeq (NFData(..), force)
import Data.ByteString.Lazy (ByteString)
import Data.Binary (decode, encode)
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import System.FilePath.Posix ((</>))
import qualified Data.ByteString.Lazy as L

import Criterion.Main (defaultMain, bgroup, bench, nf)
import Control.DeepSeq.Generics (genericRnf)
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

deriving instance Generic Bson.Value
deriving instance Generic Bson.MinMaxKey
deriving instance Generic Bson.Regex
deriving instance Generic Bson.Javascript
deriving instance Generic Bson.ObjectId
deriving instance Generic Bson.Field

instance NFData Bson.Value where
    rnf = genericRnf

instance NFData Bson.MinMaxKey where
    rnf = genericRnf

instance NFData Bson.Regex where
    rnf = genericRnf

instance NFData Bson.Javascript where
    rnf = genericRnf

instance NFData Bson.ObjectId where
    rnf = genericRnf

instance NFData Bson.Field where
    rnf = genericRnf

encodeBresson :: Bresson.Document -> ByteString
encodeBresson = encode
{-# INLINE encodeBresson #-}

decodeBresson :: ByteString -> Bresson.Document
decodeBresson = decode
{-# INLINE decodeBresson #-}

encodeBson :: Bson.Document -> ByteString
encodeBson = runPut . Bson.putDocument
{-# INLINE encodeBson #-}

decodeBson :: ByteString -> Bson.Document
decodeBson = runGet Bson.getDocument
{-# INLINE decodeBson #-}

main :: IO ()
main = do
    !(twitterBytes, twitterBson, twitterBresson) <- go "twitter100.bson"
    defaultMain [ bgroup "bson"
                  [ bgroup "twitter-100"
                    [ bench "encode" $ nf encodeBson twitterBson
                    , bench "decode" $ nf decodeBson twitterBytes
                    ]
                  ]
                , bgroup "bresson"
                  [ bgroup "twitter-100"
                    [ bench "encode" $ nf encodeBresson twitterBresson
                    , bench "decode" $ nf decodeBresson twitterBytes
                    ]
                  ]
                ]
  where
    go fileName = do
        bytes <- L.readFile $ "benchmarks" </> "bson-data" </> fileName
        return (force bytes,
                force $ decodeBson bytes,
                force $ decodeBresson bytes)
