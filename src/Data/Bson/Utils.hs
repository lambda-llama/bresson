{-# LANGUAGE OverloadedStrings #-}

module Data.Bson.Utils
    ( (!?)
    ) where

import Control.Monad (foldM)

import qualified Data.Text as ST
import qualified Data.HashMap.Strict as HashMap

import Data.Bson.Class (FromBson(..))
import Data.Bson.Instances ()
import Data.Bson.Types (BsonLabel, BsonDocument)

-- | Recursively lookup a nested field in a Document.
(!?) :: FromBson a => BsonDocument -> BsonLabel -> Maybe a
doc !? label = foldM (flip look) doc (init chunks) >>=
    look (last chunks)
  where
    chunks = ST.splitOn "." label
    look k o = HashMap.lookup k o >>= either (const Nothing) Just . fromBson
