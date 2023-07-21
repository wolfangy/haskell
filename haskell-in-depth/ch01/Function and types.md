
# Chapter 1

Chapter covers:

* Using GHCi

* Writing simple function with pure functions and I/O actions

* Using a type-based approach

* Using GHC Extensions

* Efficient processing of text data

----

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

### 1.4.1 Separating I/O from pure functions

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

### 1.4.2 Computing the most frequent words by sorting them

```haskell
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
-- requires using a comparison function, which returns the `Ordering` data type (value can be `LT`, `EQ` or `GT`).

-- comparing from Data.Ord module:
comparing :: Ord a => (b -> a) -> b -> b -> Ordering

-- To sort in descending order:
wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy (comparing $ Down . snd)
```

### 1.4.3 Formatting reports

Because we prefer working with `Text`, we need `T.pack` to:

```haskell
T.pack :: String -> Text
```

To avoid explict calls the `T.pack` function for `String` literals:

enable: **OverloadedStrings**

in source code file by starting with:

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

with GHCi:

```haskell
> :set -XOverloadedStrings
```

`Data.Text` module also supply the function to append `Text` values to each other:

```haskell
T.append :: Text -> Text -> Text
```

The issue with only `append`:

1. No any `formatting`, just manipulate `Text` value;
2. The `append` of `Text` values may be quite slow.

:point_right: The key idea of string formatting is to employ a `builder`, which is a data type responsible for collecting text components incrementally and concatenating them into `Text` value.

:point_right: The formatting text is very close to  `templating`, where you provide a template of the text with some tokens, which are substituted by values are the final stage of processing.

:package: `fmt` package

Example:

```haskell
> :set -XoverloadedStrings
> import Fmt
> name = "John"
> age = 30
> fmt $ "Hello, " +|name|+"!\nI know that your age is "+|age|+".\n"
> fmt $ "That is "+|hexF age|+ " in hex!\n"
```

:crystal_ball: The operator `+|` and `|+` are used to including variables and formatters (ordinary functions like `hexF` here)

```haskell
(+|) :: Fmt.Internal.Core.FromBuilder b => Builder -> Builder -> b

(|+) :: (Buildable a, Fmt.Internal.Core.FromBuilder b) => a -> Builder -> b

fmt :: Fmt.Internal.Core.FromBuilder b => Builder -> b

-- :exclamation:
type Buildable :: * -> Constraint
class Buildable p where build :: p -> Builder
  {-# MINIMAL build #-}
        -- Defined in ‘formatting-7.1.3:Formatting.Buildable’
instance Buildable [Char]
  -- Defined in ‘formatting-7.1.3:Formatting.Buildable’
instance [overlappable] Buildable a => Buildable [a]
  -- Defined in ‘formatting-7.1.3:Formatting.Buildable’
instance Buildable Word
  -- Defined in ‘formatting-7.1.3:Formatting.Buildable’
instance Buildable Text
  -- Defined in ‘formatting-7.1.3:Formatting.Buildable’

-- :exclamation:
type Fmt.Internal.Core.FromBuilder :: * -> Constraint
class Fmt.Internal.Core.FromBuilder a where
  Fmt.Internal.Core.fromBuilder :: Builder -> a
  {-# MINIMAL fromBuilder #-}
        -- Defined in ‘Fmt.Internal.Core’
instance (a ~ Char) => Fmt.Internal.Core.FromBuilder [a]
  -- Defined in ‘Fmt.Internal.Core’
instance Fmt.Internal.Core.FromBuilder Text
  -- Defined in ‘Fmt.Internal.Core’
```

:crystal_ball: Sometimes we need to call `show` for our variable (if the `fmt` package doesn't know how to convert it to textual form). This can be done implicitly via the other pair of operators: `+||` and `||+`.

```haskell
(+||) :: Fmt.Internal.Core.FromBuilder b => Builder -> Builder -> b

(||+) :: (Buildable a, Fmt.Internal.Core.FromBuilder b) => a -> Builder -> b
````

:crystal_ball: We can always provide a formatter for our data by writing a function returning `Builder`, which is the data type used for efficiently constructing `Text` values.

```haskell
newtype Builder = 
    Data.Text.Internal.Builder.Builder {
        Data.Text.Internal.Builder.runBuilder :: forall s. 
            (Data.Text.Internal.Builder.Buffer s -> GHC.ST.ST s [Data.Text.Internal.Text])
            -> Data.Text.Internal.Builder.Buffer s
            -> GHC.ST.ST s [Data.Text.Internal.Text]
    }

-- nameF: gives a name to the rest of the output
nameF :: Builder -> Builder -> Builder
> namedArrBuilder = nameF (build "Array" ) $ build . T.pack . show $ [1..5]
-- namedArrBuilder: "Array: [1,2,3,4,5]\n"

-- unlinesF: combine elements of the list into one Builder
unlinesF :: (Foldable f, Buildable a) => f a -> Builder
> linesBuilder = unlinesF $ fmap (nameF "item" . build . T.pack . show) [1..5]
> fmt linesBuilder

-- linesBuilder :
-- item: 1
-- item: 2
-- item: 3
-- item: 4
-- item: 5

-- blockListF': formats list elements in the given way and presents them line by line
blockListF' :: Foldable f => Text -> (a -> Builder) -> f a -> Builder
blockList :: (Foldable f, Buildable a) => f a -> Builder

> pairs = [p | p <- zip [1..5] $ tail [1..5]]
> pairBuilder (a, b) = "(" +|a|+ ", " +|b|+ ")"
> fmt $ blockListF' "> " pairBuilder paris
-- > (1, 2)
-- > (2, 3)
-- > (3, 4)
-- > (4, 5)
```

### 1.4.4 Rule them all with IO Actions

Note `when` function from `Control.Monad` module: It allows printing the corresponding report if the user request it.

```haskell
when :: Applicative f => Bool -> f () -> f ()
```
