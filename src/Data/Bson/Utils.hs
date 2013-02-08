{-# LANGUAGE OverloadedStrings #-}

module Data.Bson.Utils
    ( document
    , (!?)
    , (=:)
    ) where

import Control.Monad (foldM)
import Prelude hiding (lookup)

import qualified Data.Text as ST
import qualified Data.HashMap.Strict as HashMap

import Data.Bson.Class (FromBson(..), ToBson(..))
import Data.Bson.Instances ()
import Data.Bson.Types (BsonLabel, BsonDocument, BsonField)

document :: [BsonField] -> BsonDocument
document = HashMap.fromList
{-# INLINE document #-}

lookup :: FromBson a => BsonLabel -> BsonDocument -> Maybe a
lookup label doc = do
    v <- HashMap.lookup label doc
    either (const Nothing) Just $! fromBson v

-- | Recursively lookup a nested field in a Document.
(!?) :: FromBson a => BsonDocument -> BsonLabel -> Maybe a
doc !? label = do
    inner <- foldM (flip lookup) doc (init chunks)
    lookup (last chunks) inner
  where
    chunks :: [BsonLabel]
    chunks = ST.splitOn "." label
{-# INLINE (!?) #-}

(=:) :: ToBson a => BsonLabel -> a -> BsonField
label =: v = (label, toBson v)
{-# INLINE (=:) #-}
