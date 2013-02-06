{-# LANGUAGE OverloadedStrings #-}

module Data.Bson.Utils
    ( document
    , (!?)
    , (=:)
    ) where

import Control.Monad (foldM)

import qualified Data.Text as ST
import qualified Data.HashMap.Strict as HashMap

import Data.Bson.Class (FromBson(..), ToBson(..))
import Data.Bson.Instances ()
import Data.Bson.Types (BsonLabel, BsonDocument, BsonField)

document :: [BsonField] -> BsonDocument
document = HashMap.fromList

-- | Recursively lookup a nested field in a Document.
(!?) :: FromBson a => BsonDocument -> BsonLabel -> Maybe a
doc !? label = foldM (flip look) doc (init chunks) >>=
    look (last chunks)
  where
    chunks = ST.splitOn "." label
    look k o = HashMap.lookup k o >>= either (const Nothing) Just . fromBson

(=:) :: ToBson a => BsonLabel -> a -> BsonField
k =: v = (k, toBson v)
