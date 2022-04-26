module Main where

import Data.Ord
import Data.List(sortBy)
import Data.Text(Text)

import qualified Data.Text.IO as TIO
import Control.Monad
import System.Environment

import Fmt

import Ch01Lib

allWordsReport :: Vocabulary -> Text
allWordsReport vocab = fmt $ nameF "All words" $ unlinesF (allWords vocab)

allWords :: Vocabulary -> [Text]
allWords = map fst

wordsCountReport :: Vocabulary -> Text
wordsCountReport vocab = fmt $
    "Total number of worlds: " +|total|+ "\nNumber of unique words: " +|unique|+ "\n"
    where
        (total, unique) = wordsCount vocab

wordsCount :: Vocabulary -> (Int, Int)
wordsCount vocab = (foldr (+) 0 $ map snd vocab, length vocab)

frequentWordReport :: Vocabulary -> Int -> Text
frequentWordReport vocab num = 
    fmt $ nameF "Frequent words"
        $ blockListF' "" fmtEntry reportData
    where
        reportData = take num $ wordsByFrequency vocab
        fmtEntry (t, n) = "" +|t|+": " +|n|+ ""

wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy (comparing $ Down . snd)

processTextFile :: FilePath -> Bool -> Int -> IO ()
processTextFile fname withAllWords n = do
    text <- TIO.readFile fname
    let vocab = extractVocab text
    when withAllWords $ TIO.putStrLn $ allWordsReport vocab
    TIO.putStrLn $ wordsCountReport vocab
    TIO.putStrLn $ frequentWordReport vocab n

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-a", fname, num] -> 
            processTextFile fname True (read num)
        [fname, num] -> 
            processTextFile fname False (read num)
        _ -> putStrLn "Usage: vocab3 [-a] filename freq_words_num"