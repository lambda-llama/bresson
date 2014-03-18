{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Bson.Document
       (Document
       , fromList
       , foldlWithKey'
       , size
       , lookup
       , lookupDefault) where

import Prelude hiding (lookup)

import Control.DeepSeq (NFData(..))

import qualified Data.HashMap.Strict as HashMap

import Data.Bson.Instances ()

import {-# SOURCE #-} Data.Bson.Types (Label, Value)

newtype Document = Document { unDocument :: HashMap.HashMap Label Value }
    deriving (Eq, Show, NFData)

foldlWithKey' ::  (a -> Label -> Value -> a) -> a -> Document -> a
foldlWithKey' f a = HashMap.foldlWithKey' f a . unDocument

fromList :: [(Label, Value)] -> Document 
fromList = Document . HashMap.fromList

size :: Document -> Int
size = HashMap.size . unDocument

lookup :: Label -> Document -> Maybe Value
lookup k = HashMap.lookup k . unDocument

lookupDefault :: Value -> Label -> Document -> Value
lookupDefault d k = HashMap.lookupDefault d k . unDocument
