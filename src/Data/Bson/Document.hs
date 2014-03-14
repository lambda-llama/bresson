module Data.Bson.Document
       (Document
       , fromList
       , foldlWithKey'
       , size
       , lookup
       , lookupDefault) where
import Prelude hiding (lookup)

import Data.Hashable(Hashable)
import Control.DeepSeq (NFData(..))

import qualified Data.Bson.HashDocument as DocImpl


newtype Document k v = Document (DocImpl.Document k v)
                     deriving (Show, Eq)

foldlWithKey' ::  (a -> k -> v -> a) -> a -> Document k v -> a
foldlWithKey' f z (Document doc)= DocImpl.foldlWithKey' f z doc

fromList :: (Hashable k, Ord k) => [(k, a)] -> Document k a
fromList = Document . DocImpl.fromList

size :: (Document k v) -> Int
size (Document doc) = DocImpl.size doc

lookup :: (Eq k, Hashable k) => k -> Document k v -> Maybe v
lookup k (Document doc) = DocImpl.lookup k doc

lookupDefault :: (Eq k, Hashable k) => v -> k -> Document k v -> v
lookupDefault v k (Document doc)= DocImpl.lookupDefault v k doc

instance (NFData v, NFData k) => NFData (Document k v) where
  rnf (Document doc) = rnf doc
