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
import Data.Bson.Parser (Parser)
import Data.Bson.Types (Label, Document, Field)

document :: [Field] -> Document
document = HashMap.fromList
{-# INLINE document #-}

lookup :: FromBson a => Label -> Document -> Parser a
lookup label doc = let mbValue = HashMap.lookup label doc in case mbValue of
    Just v -> fromBson v
    Nothing -> fail $ "Key " ++ show label ++ " not found"
{-# INLINE lookup #-}

-- | Recursively lookup a nested field in a Document.
(!?) :: FromBson a => Document -> Label -> Parser a
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
