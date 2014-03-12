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
import Data.Bson.Parser (parseMaybe)
import Data.Bson.Types (Label, Document, Field)

document :: [Field] -> Document
document = HashMap.fromList
{-# INLINE document #-}

lookup :: FromBson a => Label -> Document -> Maybe a
lookup label doc = do
    v <- HashMap.lookup label doc
    parseMaybe fromBson v

-- | Recursively lookup a nested field in a Document.
(!?) :: FromBson a => Document -> Label -> Maybe a
doc !? label = do
    inner <- foldM (flip lookup) doc (init chunks)
    lookup (last chunks) inner
  where
    chunks :: [Label]
    chunks = ST.splitOn "." label
{-# INLINE (!?) #-}

(=:) :: ToBson a => Label -> a -> Field
label =: v = (label, toBson v)
{-# INLINE (=:) #-}
