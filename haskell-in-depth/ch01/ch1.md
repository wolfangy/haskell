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

**:bomb:The results we got in GHCi show that we've forgotten about the leading and trailing punctuation...**

```haskell
> text <- readFile "data/texts/hamlet.txt"
> ws = words $ map toLower text
-- ws :: [String]
> ws' = map (takeWhile isLetter . dropWhile (not . isLetter)) ws
-- isLetter :: Char -> Bool
-- not :: Bool -> Boo

-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- dropWhile :: (a -> Bool) -> [a] -> [a]

-- use `dropWhile (not . isLetter)` to remove the leading punctuation
-- use `takeWhile (isLetter)` to drop the trailing punctuation

-- ws' :: [String]

> cleanedWords = filter (not . null) ws'
-- null :: Foldable t => t a -> Bool
-- `filter (not .null)` to filter out the empty string

> uniqueWords = map head $ group $ sort cleanedWords

> length uniqueWords
```

**:bomb: The seconde parts of all hyphenated words have been removed due to `takeWhile isLetter`**

## 1.2 From GHCi and String to GHC and Text

Replacing `String` with much more efficient `Text`:

**Example: ch01/vocab1.hs**

:exclamation: The module `Data.Text` and `Data.Text.IO` are usually imported with qualifiers to avoid name clashes with `Prelude`; 

:exclamation: These two modules come with the `text` package.

