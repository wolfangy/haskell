{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module IniParse where

-- stack ghci --package trifecta --package raw-strings-qq --package hspec --package text --package containers --package bytestring -- .\IniParse.hs


import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec

import Text.RawString.QQ
import Text.Trifecta

headerEx :: String
headerEx = "[blah]"

assignmentEx :: String
assignmentEx = "woot=1"

commentEx :: String
commentEx = 
    "; last modified 1 April\
    \ 2001 by John Doe"

commentEx' :: String
commentEx' =
    "; blah\n; woot\n \n;hah"

sectionEx :: String
sectionEx =
    "; ignore me\n[states]\nChris=Texas"

sectionEx' :: String
sectionEx' = [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx'' :: String
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw

[whatisit]
red=intoothandclaw
|]

-- Header
--
newtype Header =
    Header String deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = 
    char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

-- Assignments
--
type Name = String
type Value = String
type Assignments = Map Name Value

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

parseAssignment :: Parser (Name, Value)
parseAssignment = do
    name <- some letter
    char '='
    val <- some (noneOf "\n")
    skipEOL
    return (name, val)


parseAssignment' :: Parser (Name, Value)
parseAssignment' = do
    name <- some letter
    char '='
    val <- some (noneOf "\n")
    return (name, val)

s = "key=value\nblah=123"

spa' = some parseAssignment'
spa = some parseAssignment

-- Comments
--
skipComments :: Parser ()
skipComments =
    skipMany (do 
        char ';' <|> char '#'
        skipMany (noneOf "\n")
        skipEOL)

-- Sections
--
data Section =
    Section Header Assignments
    deriving (Eq, Show)

newtype Config =
    Config (Map Header Assignments)
    deriving (Eq, Show)

skipWhiteSpace :: Parser ()
skipWhiteSpace =
    skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
    skipWhiteSpace
    skipComments

    h <- parseHeader
    skipEOL
    assignments <- some parseAssignment
    return $ Section h (M.fromList assignments)

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m =
    M.insert h a m

parseIni :: Parser Config
parseIni = do
    sections <- some parseSection
    let mapOfSections = foldr rollup M.empty sections
    return (Config mapOfSections)
