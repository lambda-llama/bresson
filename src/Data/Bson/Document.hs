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

import {-# SOURCE #-} Data.Bson.Types (Label, Value)
import Data.Bson.Instances ()
import qualified Data.Bson.HashDocument as DocImpl

newtype Document = Document (DocImpl.Document Label Value)
                     deriving (Show, Eq)

foldlWithKey' ::  (a -> Label -> Value -> a) -> a -> Document -> a
foldlWithKey' f z (Document doc)= DocImpl.foldlWithKey' f z doc

fromList :: [(Label, Value)] -> Document
fromList = Document . DocImpl.fromList

size :: Document -> Int
size (Document doc) = DocImpl.size doc

lookup :: Label -> Document -> Maybe Value
lookup k (Document doc) = DocImpl.lookup k doc

lookupDefault :: Value -> Label -> Document -> Value
lookupDefault v k (Document doc)= DocImpl.lookupDefault v k doc

instance NFData Document where
  rnf (Document doc) = rnf doc
