# Chapter 1 Function and types

Chapter covers:

* Using GHCi

* Writing simple function with pure functions and I/O actions

* Using a type-based approach

* Using GHC Extensions

* Efficient processing of text data

## 1.1 Solving problems in GHCi REPL

### Task

* Extract all the words from a given text file
* Count the number of unique words
* Find the most frequently used words

```haskell
-- with `ghci` repl:
> :module + Data.List Data.Char
> text <- readFile "data/texts/hamlet.txt"
> ws = map head $ group $ sort $ words $ map toLower text
```

```haskell
toLower :: Char -> Char

words :: String -> [String]

sort :: Ord a => [a] -> [a]

group :: Eq a => [a] -> [[a]]
```

**The results we got in GHCi show that we've forgotten about the leading and \trailing punctuation...**

