{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Criterion.Main
import           System.Directory
import qualified Data.Text.IO as T
import           Data.CSS.Syntax.Tokenizer



main :: IO ()
main = do
    cwd <- getCurrentDirectory
    tokenizeBenchmarks <- do
        websites <- drop 2 <$> getDirectoryContents (cwd ++ "/benchmark/fixtures")
        forM websites $ \website -> do
            files <- drop 2 <$> getDirectoryContents (cwd ++ "/benchmark/fixtures/" ++ website)
            benchmarks <- forM files $ \file -> do
                body <- T.readFile $ cwd ++ "/benchmark/fixtures/" ++ website ++ "/" ++ file
                pure $ bench file $ whnf tokenize body

            pure $ bgroup website benchmarks

    defaultMain
        [ bgroup "tokenize" tokenizeBenchmarks
        ]
