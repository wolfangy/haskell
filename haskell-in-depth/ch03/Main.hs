{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Foldable (toList)

import Data.Csv (decodeByName)

import QuoteData

main :: IO ()
main = putStrLn "Stock quotes processing project"

readQuotes :: FilePath -> IO [QuoteData]
readQuotes fpath = do
    csvData <- BL.readFile fpath
    case decodeByName csvData of
        Left err -> error err
        Right (_, quotes) -> pure (toList quotes)