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

:exclamation: The module `Data.Text` and `Data.Text.IO` are usually imported with qualifiers to avoid name clashes with `Prelude`.

:exclamation: These two modules come with the `text` package.

```haskell

T.dropAround :: (Char -> Bool) -> T.Text -> T.Text
-- dropAround to remove the leading and tailing characters

T.toCaseFold :: T.Text -> T.Text
-- toCaseFold converts the whole `Text` value to the folded case and does that significantly faster than mapping with `toLower` over every character.
-- toCaseFold mainly useful for performing caseless string comparisons.
```

## 1.3 Functional program as sets of IO actions

**Example: ch01/vocab2.hs**

```haskell
type Entry = (T.Text, Int)
type Vocabulary = [Entry]

extractVocab :: T.Text -> Vocabulary
printAllWords :: Vocabulary -> IO ()
processTextFile :: FilePath -> IO ()

main :: IO ()
```

1. Extracting a vocabulary from the file's context
2. Using the vocabulary to print all words

:exclamation: The `extractVocab` is the only `pure` function in this program.

:wilted_flower: The program stick with `IO` so much that almost every 
function in the program is an I/O action.

## 1.4 Embracing pure functions

The role of `pure` functions:

* They are easier to combine with other functions

* They cannot break anything in other parts of the program.

* Their correctness can be reasoned

**Example ch01/vocab3.hs**

```haskell
extractVocab :: Text -> Vocabulary

allWordsReport      :: Vocabulary -> Text
wordsCountReport    :: Vocabulary -> Text
frequentWordsReport :: Vocabulary -> Int -> Text

processTextFile :: File -> Bool -> Int -> IO ()
main :: IO ()
```

:thumbsup: **Qualifying imports**
We want use `Text` identifier without explicit qualification but keep mandatory qualification for all the other identifiers from this module:

```haskell
import Data.Text(Text)
import qualified Data.Text as T
```
