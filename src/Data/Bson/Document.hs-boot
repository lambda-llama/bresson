module Data.Bson.Document (Document) where

import Control.DeepSeq (NFData)

import Data.HashMap.Strict (HashMap)

import {-# SOURCE #-} Data.Bson.Types (Label, Value)

newtype Document = Document (HashMap Label Value)

instance Eq Document
instance Show Document
instance NFData Document