{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Applicative
import Data.Ratio ((%))
import Text.RawString.QQ
import Text.Trifecta

-- stack ghci --package trifecta --package raw-strings-qq -- .\Main.hs
--

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneEof = one >> eof

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

oneTwoEof = oneTwo >> eof


testParse :: Parser Char -> IO ()
testParse p =
    print $ parseString p mempty "123"

pNL s = putStrLn ('\n':s)

badFraction = "1/0"
alsoBad = "10"

shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal

    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return (numerator % denominator)


parseInteger :: Parser Integer
parseInteger = do
    val <- integer
    eof
    return val

-- AltParsing
--
type NumberOrString =
    Either Integer String

a = "blah"

b = "123"

c = "123blah789"

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

-- QuasiQuotes
-- `[r|` begins a quasiquoted section, using the quasiquoter named r.
--

parseNos :: Parser NumberOrString
parseNos = 
    (Left <$> integer) <|> (Right <$> some letter)

parseNosSkip :: Parser NumberOrString
parseNosSkip = 
    skipMany (oneOf "\n") >>
        (Left <$> integer) <|> (Right <$> some letter)

parseNosSkipSkip :: Parser NumberOrString
parseNosSkipSkip = do
    skipMany (oneOf "\n") 
    v <- (Left <$> integer) <|> (Right <$> some letter)
    skipMany (oneOf "\n")
    return v

-- `<|>` can read as being an or, or disjunction, of our two parsers
--
-- `many` is zero or more
-- `some` is one or more
--

data MyName = MyName String deriving Show

someLetter = some letter :: Parser String

nameParser = MyName <$> someLetter

chrisParsed = parseString nameParser mempty "Chris"

main :: IO ()
main = do
    pNL "stop:"
    testParse stop

    pNL "one:"
    testParse one

    pNL "one':"
    testParse one'

    pNL "oneTwo:"
    testParse oneTwo

    pNL "oneTwo':"
    testParse oneTwo'

    putStrLn "------ Parse Fraction ------"

    let parseFraction' = parseString parseFraction mempty

    print $ parseFraction' shouldWork
    print $ parseFraction' badFraction
    print $ parseFraction' shouldAlsoWork
    print $ parseFraction' alsoBad

    print $ parseString integer mempty "123"
    print $ parseString (integer >> eof) mempty "123"
    print $ parseString parseInteger mempty "123"
    print $ parseString (integer >>= \i -> eof >>= \_ -> return i) mempty "123"

    putStrLn "------ AltParsing ------"

    let p f i = parseString f mempty i

    print $ p (some letter) a
    print $ p integer b
    print $ p (some integer) b

    print $ p parseNos a
    print $ p parseNos b
    print $ p parseNos c

    print $ p (many parseNos) c
    print $ p (some parseNos) c

    print $ p parseNos eitherOr
    print $ p parseNosSkip eitherOr
    print $ p (some parseNosSkip) eitherOr
    print $ p (some parseNosSkipSkip) eitherOr
    print $ p (some (token parseNosSkip)) eitherOr
-- token parsers and character parsers are different sors of things
-- applying `token` to `parseNos` did for us here is make it optionally consume
-- trailing whitespsaces we don't care about.
