module Data.Bson.HashDocument
       (Document
       , foldlWithKey'
       , fromList
       , size
       , lookup
       , lookupDefault
       )where

import Prelude hiding (lookup)

import Data.Hashable(Hashable)
import qualified Data.HashMap.Strict as HashMap

type Document k v = HashMap.HashMap k v

foldlWithKey' ::  (a -> k -> v -> a) -> a -> Document k v -> a
foldlWithKey' = HashMap.foldlWithKey'

fromList :: (Hashable k, Ord k) => [(k, a)] -> Document k a
fromList = HashMap.fromList

size :: Document k v -> Int
size = HashMap.size

lookup :: (Eq k, Hashable k) => k -> Document k v -> Maybe v
lookup = HashMap.lookup

lookupDefault :: (Eq k, Hashable k) => v -> k -> Document k v -> v
lookupDefault = HashMap.lookupDefault
