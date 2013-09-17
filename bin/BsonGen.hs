{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_)
import Data.Binary (encode)
import System.FilePath ((</>), (<.>))
import System.Directory (createDirectoryIfMissing)
import System.Random (newStdGen, randomIO)

import Options.Applicative (ParserInfo, info, execParser, option,
                            long, helper, fullDesc, value,
                            metavar, showDefault, argument, str,
                            showDefaultWith, (<>))
import Test.QuickCheck (Arbitrary, arbitrary, resize)
import Test.QuickCheck.Gen (unGen)
import qualified Data.ByteString.Lazy as LazyByteString

import Data.Bson (Document)
import Data.Bson.Tests.Instances ()

data Options = Options
    { optSeed  :: Maybe Int
    , optSize  :: Int
    , optFiles :: Int
    , optDest  :: FilePath
    } deriving (Show, Eq, Ord)

options :: ParserInfo Options
options = info (helper <*> parser) fullDesc
  where
    parser = Options
        <$> option ( long "seed"
        	       <> value Nothing
                   <> metavar "NUMBER"
        		   <> showDefaultWith (const "random"))
        <*> option ( long "size"
                   <> value 100
                   <> metavar "NUMBER"
                   <> showDefault)
        <*> option ( long "files"
                   <> value 100
                   <> metavar "NUMBER"
                   <> showDefault)
        <*> argument str ( metavar "FILE" )

generate :: Int -> Int -> IO Document
generate size seed = newStdGen >>= \gen ->
    return $ unGen (resize size arbitrary) gen seed

main :: IO ()
main = do
    Options { .. } <- execParser $ options
    seed <- case optSeed of
        Just s -> return s
        Nothing -> randomIO

    createDirectoryIfMissing True optDest

    forM_ [1..100] $ \(n :: Int) -> do
        let file = optDest </> show n <.> "bson"
        generate optSize seed >>= LazyByteString.writeFile file . encode
