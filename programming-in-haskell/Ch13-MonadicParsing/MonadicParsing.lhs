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