{-# LANGUAGE OverloadedStrings #-}
 
module BT where

import Control.Applicative
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString (parseOnly)

import Data.ByteString (ByteString)
import Text.Trifecta hiding (parseTest)
import Text.Parsec (Parsec, parseTest)

-- stack ghci --package trifecta --package attoparsec --package parsec --package bytestring -- ./Backtrack.hs

trifP :: Show a => Parser a -> String -> IO ()
trifP p i = print $ parseString p mempty i

parsecP :: (Show a) => Parsec String () a -> String -> IO ()
parsecP = parseTest

attoP :: Show a => A.Parser a -> ByteString -> IO ()
attoP p i = print $ parseOnly p i

nobackParse :: (Monad f, CharParsing f) => f Char
nobackParse = (char '1' >> char '2') <|> char '3'

-- Not backtrack: it attempts to parse '1' followed by '2' or '3'

tryParse :: (Monad f, CharParsing f) => f Char
tryParse = try (char '1' >> char '2') <|> char '3'

-- it backtracks if the first parse fails.
-- Backtracking: the input cursor returns to where it was before the failed parser consumed input
--

tryAnnot :: (Monad f, CharParsing f) => f Char
tryAnnot = 
    (try (char '1' >> char '2') <?> "Tried 12")
    <|> (char '3' <?> "Tried 3")


main :: IO ()
main = do
    putStrLn "------ Trifecta ------"
    trifP nobackParse "13"
    trifP tryParse "13"
    trifP tryParse "12"
    trifP tryParse "3"
    trifP tryAnnot "13"

    putStrLn "------ Parsec ------"
    parsecP nobackParse "13"
    parsecP tryParse "13"
    parsecP nobackParse "3"
    parsecP tryParse "12"
    parsecP tryAnnot "13"

    putStrLn "------ Attoparsec ------"
    attoP nobackParse "13"
    attoP tryParse "13"
    attoP nobackParse "3"
    attoP tryParse "12"
    attoP tryAnnot "13"
