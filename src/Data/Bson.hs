module Data.Bson
    ( module Data.Bson.Class
    , module Data.Bson.Parser
    , module Data.Bson.Types
    , module Data.Bson.Utils
    , fromList -- NOTE(matklad) this is a problem =(
    ) where

-- Instance Binary Document
import Data.Bson.Binary ()

import Data.Bson.Class (ToBson(..), FromBson(..))
import Data.Bson.Document(fromList)
import Data.Bson.Parser (Parser, parse, parseMaybe, parseEither)
import Data.Bson.Types (Document, Label, Value(..), Binary(..), ObjectId(..),
						Array, RegexOption(..), RegexOptions)
import Data.Bson.Utils (document, (!?), (=:))
