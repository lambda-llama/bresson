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

import {-# SOURCE #-} Data.Bson.Types (Label, Value)

type Document = HashMap.HashMap Label Value

foldlWithKey' ::  (a -> Label -> Value -> a) -> a -> Document -> a
foldlWithKey' = HashMap.foldlWithKey'

fromList :: [(Label, Value)] -> Document 
fromList = HashMap.fromList

size :: Document -> Int
size = HashMap.size

lookup :: Label -> Document -> Maybe Value
lookup = HashMap.lookup

lookupDefault :: Value -> Label -> Document -> Value
lookupDefault = HashMap.lookupDefault
