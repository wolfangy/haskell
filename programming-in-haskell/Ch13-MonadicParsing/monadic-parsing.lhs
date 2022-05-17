> {-# LANGUAGE GADTs #-}

> import Control.Applicative
> import Data.Char

13.1 What is a parser

A parser is a program that takes a string of character as input, and produces some 
form of tree that makes the syntactic structure of the string explicit.

!!! Parsers are an important topic in computing, because most real-life programs 
use a parser to preprocess their input.

13.2 Parsers as functions

In haskell, a parser can naturally be viewed directly as a function that takes a 
string adn produces a tree. 

    type Parser = String -> Tree

however, a parser might not always consume its entire argument string, we generalise 
our type to also return any unconsumed part of the argument string:

    type Parser = String -> (Tree, String)

Similarly, a parser might not always succeed. We further generalise our type for 
parsers to return a list of results, with the convention taht the empyt list denotes 
failure, and a singleton list denotes success:

    type Parser = String -> [(Tree, String)]

noting:
    
    String -> [(a, String)]

    State  -> (a, State)

The key difference is that a parser also has the possibility to fail by resulting 
a list of results.

13.3 Basic definitions

> newtype Parser a = P (String -> [(a, String)])

> parse :: Parser a -> String -> [(a, String)]
> parse (P p) inp = p inp

> item :: Parser Char
> item = P (\inp -> case inp of
>                       [] -> []
>                       (x:xs) -> [(x, xs)])

13.4 Sequencing parsers

> instance Functor Parser where
>   -- fmap :: (a -> b) -> Parser a -> Parser b
>   fmap f p = P (\inp -> let r = parse p inp
>                         in 
>                            [(f a, s) | (a, s) <- r])

> instance Applicative Parser where
>   pure a = P (\inp -> [(a, inp)])

    (<*>) :: forall a b. Parser (a -> b) -> Parser a -> Parser b

>   f <*> p = P (\inp -> let
>                               pf = parse f inp
>                            in
>                               [(g a, s') | (g, s) <- pf, (a, s') <- parse p s])

> threeA :: Parser (Char, Char)
> threeA = pure g <*> item <*> item <*> item
>   where
>       g x y z = (x, z)

> instance Monad Parser where
>   p >>= f = P (\inp -> let
>                           v = parse p inp
>                        in
>                           [(b, s') | (a, s) <- v, (b, s') <- parse (f a) s])

> threeM :: Parser (Char, Char)
> threeM = do
>   x <- item
>   item
>   z <- item
>   return (x, z)

13.5 Making choices

Option 1: The `do` notation combines parsers in sequence, with the output string 
from each parser in the sequence becoming the input string for the next.

Option 2: To apply one parser to the input string, and if this fails to then apply 
another to the same input instead.

Make a choice between two alternatives can be generalised to a range of applicative 
types.

    class Applicative f => Alternative f where
        empty :: f a
        (<|>) ::f a -> f a -> f a

`empty` represents an alternative that has failed.
`(<|>)` is an appropriate choice operator for the type.

Laws:
     
    empty <|> x = x

    x <|> empty = x

    x <|> (y <|> z) = (x <|> y) <|> z


The motivating example of an `Alternative` type is `Maybe`

    instance Alternative Maybe where
        -- empty is given by the failure
        empty = Nothing
        
        -- (<|>) returns the first argument if this succeeds
        Nothing <|> my = my
        (Just x) <|> _ = Just x

The instance for the `Parser` type is a natural extension of this idea, where
`empty` is the parser that always fails regardless of the input string
`<|>` is a choice operator that returns the result of the first parser if it succeeds 
    on the input, and applies the second parser to the same input otherwise


> instance Alternative Parser where
>   empty = P (\inp -> [])
>   p <|> q = P (\inp -> case parse p inp of
>                           []          -> parse q inp
>                           [(v, out)]  -> [(v, out)])

> always_d  = parse (empty <|> return 'd') "abc"
> not_d     = parse (item  <|> return 'd') "abc"


13.6 Derived primitives

Three basic parsers:

1. `item` consumes a single character

2. `return v` always succeeds with the result `v`

3. `empty` always fails

> sat :: (Char -> Bool) -> Parser Char
> sat p = do
>   x <- item
>   if p x then return x else empty

Using `sat` and appopriate predicates from `Data.Char`, we can define parsers for 
single digits, lower-case letters, upper-case letters, arbitrary letters, alpahnumeric 
characters, and specific character:

> digit :: Parser Char
> digit = sat isDigit

> lower :: Parser Char
> lower = sat isLower

> upper :: Parser Char
> upper = sat isUpper

> letter :: Parser Char
> letter = sat isAlpha

> alphanum :: Parser Char
> alphanum = sat isAlphaNum

> char :: Char -> Parser Char
> char x = sat (== x)

using `char` we can define a parser `string xs` for the string of characters `xs`

> string :: String -> Parser String
> string [] = return []
> string (x:xs) = do
>   char x
>   string xs
>   return (x:xs)

!!! `string` only succeeds if the entire target string is consumed from the input.

There is no need to define `many` and `some` ourselves, as suitable default definitions 
are already provided by the `Alternative` class

`many p` - apply a parser `p` as many times as possible until it fails, with the 
values from each successful application of `p` being returned in a list.

    many :: f a -> f [a]
    many x = some x <|> pure []

`some p` - requires at least one successful application.

    some :: f a -> f [a]
    some x = pure (:) <*> x <*> many x

use `many` and `some` to define more parsers:

parser for identifiers (variable names)

> ident :: Parser String
> ident = do
>   x <- lower
>   xs <- many alphanum
>   return (x:xs)

To allow the initial char as `_`

> ident' :: Parser String
> ident' = do
>   x <- (some $ char '_' ) <|> some lower
>   xs <- many alphanum
>   return $ x ++ xs

parser for natual numbers

> nat :: Parser Int
> nat = do
>   xs <- some digit
>   return (read xs)


parser for spacing (comprising zero or more space)

> space :: Parser ()
> space = many (sat isSpace) >> return ()

using `nat` to define a parser for integer

> int :: Parser Int
> int = do
>          char '-'
>          n <- nat
>          return (-n)
>       <|> nat

13.7 Handling spacing

To handle (ignore) spacing around the basic token

> token :: Parser a -> Parser a
> token p = do
>               space
>               v <- p
>               space
>               return v

use `token` to define parsers that ignore spacing around:

> identifier :: Parser String
> identifier = token ident

> natural :: Parser Int
> natural = token nat

> integer :: Parser Int
> integer = token int

> symbol :: String -> Parser String
> symbol xs = token (string xs)

a parser for a non-empty list of natural numbers

> nats :: Parser [Int]
> nats = do
>   symbol "["
>   n <- natural
>   ns <- many (do symbol ","
>                  natural)
>   symbol "]"
>   return (n:ns)

13.8 Arithmetic expressions

a grammar for our language of arithmetic expressions can be defined by:

    expr ::= expr + expr | expr * expr | ( expr ) | nat
    nat  ::= 0 | 1 | 2 | ..

The problem of the grammar for expressions does not take account of the fact that 
multiplication has higher priority than addition.

To address this problem:

    expr ::= expr + expr | term
    term ::= term * term | factor
    factor ::= ( expr ) | nat
    nat ::= 0 | 1 | 2 | ...

separate rule for each level of priority, with:

* addition at the lowest level of priority,

* multiplication at the middle level,

* parentheses and numbers at the highest level.

The problem of the grammar: does not take account of the fact that addition and 
multiplication associate to the right.

To address the problem:

    expr ::= term + expr | term
    term ::= factor * term | factor
    factor ::= ( expr ) | nat
    nat ::= 0 | 1 | 2 | ...

our grammar for expressions is now *unambiguous*, in the sense that every well-formed 
expression has precisely one parse tree.

Final modification to the grammar is a simplification.

the symbol `ε` denotes the empty string.

    expr ::= term ( + expr | ε)
    term ::= factor ( * term | ε)
    factor ::= ( expr ) | nat
    nat ::= 0 | 1 | 2 | ...

Translate the grammar into a parser

> expr :: Parser Int
> expr = do
>   t <- term
>   do
>       symbol "+"
>       e <- expr
>       return (t + e)
>    <|> return t

> term :: Parser Int
> term = do
>   f <- factor
>   do
>       symbol "*"
>       t <- term
>       return (f * t)
>    <|> return f

> factor :: Parser Int
> factor = do symbol "("
>             e <- expr
>             symbol ")"
>             return e
>          <|> natural

> eval :: String -> Int
> eval xs = case (parse expr xs) of
>               [(n, [])]  -> n
>               [(_, out)] -> error ("Unused input " ++ out)
>               []         -> error "Invalid input"

13.9 Calculator

user interface:

> cls :: IO ()
> cls = putStr "\ESC[2J"

> goto :: Pos -> IO()
> goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

> beep :: IO()
> beep = putStr "\BEL"

> type Pos = (Int, Int)

> writeat :: Pos -> String -> IO ()
> writeat p xs = do
>    goto p
>    putStr xs

> box :: [String]
> box = ["+---------------+",
>        "|               |",
>        "+---+---+---+---+",
>        "| q | c | d | = |",
>        "+---+---+---+---+",
>        "| 1 | 2 | 3 | + |",
>        "+---+---+---+---+",
>        "| 4 | 5 | 6 | - |",
>        "+---+---+---+---+",
>        "| 7 | 8 | 9 | * |",
>        "+---+---+---+---+",
>        "| 0 | ( | ) | / |",
>        "+---+---+---+---+" ]

> buttons :: String
> buttons = standard ++ extra
>   where
>       standard = "qcd=123+456-789*0()/"
>       extra    = "QCD \ESC\BS\DEL\n"

> showbox :: IO ()
> showbox = sequence_ [writeat (1, y) b | (y, b) <- zip [1..] box]

> display xs = do
>   writeat (3, 2) (replicate 13 ' ')
>   writeat (3, 2) (reverse (take 13 (reverse xs)))

> calc :: String -> IO ()
> calc xs = do 
>   display xs
>   c <- getChar
>   if elem c buttons then process c xs
>   else do
>           beep
>           calc xs

> process :: Char -> String -> IO ()
> process c xs 
>   | elem c "qQ\ESC"       = quit
>   | elem c "dD\BS\DEL"    = delete xs
>   | elem c "=\n"          = eval' xs
>   | elem c "cC"           = clear
>   | otherwise             = press c xs

> quit :: IO ()
> quit = goto (1, 14)

> delete :: String -> IO ()
> delete [] = calc []
> delete xs = calc (init xs)

> eval' :: String -> IO ()
> eval' xs = case parse expr xs of
>               [(n, [])]   -> calc (show n)
>               _           -> do beep
>                                 calc xs

> clear :: IO ()
> clear = calc []

> press :: Char -> String -> IO ()
> press c xs = calc (xs ++ [c]) 

> run :: IO ()
> run = do
>   cls
>   showbox
>   clear
