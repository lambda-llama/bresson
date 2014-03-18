{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative ((<$>), (<*>))
import Control.DeepSeq (deepseq)
import Data.Binary (decode)

import Options.Applicative (ParserInfo, info, execParser, helper, fullDesc,
                            metavar, argument, str,)
import qualified Data.ByteString.Lazy as LazyByteString

import Data.Bson (Document)

data Options = Options
    { optFile  :: FilePath
    } deriving (Show, Eq, Ord)

options :: ParserInfo Options
options = info (helper <*> parser) fullDesc
  where
    parser = Options <$> argument str ( metavar "FILE" )

decodeDocument :: LazyByteString.ByteString -> Document
decodeDocument = {-# SCC decodeDocument #-} decode

main :: IO ()
main = do
    Options { .. } <- execParser $ options
    content <- LazyByteString.readFile optFile
    decodeDocument content `deepseq` return ()