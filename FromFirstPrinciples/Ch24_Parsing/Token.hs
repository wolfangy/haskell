module Token where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

-- stack ghci --package trifecta --package raw-strings-qq -- .\Main.hs

p i j = parseString i mempty j
t = token

s = "123 \n \n 456"

d1 = p (some digit) "123 456"
-- "123"

d1' = p (some (some digit)) "123 456"
-- ["123"]

d2 = p (t (some digit)) s
-- "123"

d2' = p (some (t digit)) s
-- "123456"

d3 = p (t (some (t digit))) s
-- "123456"

d4 = p (some (t (some digit))) s
-- ["123", "456"]

dd1 = p (some decimal) s
-- [123]

dd2 = p (some (t decimal)) s
-- [123, 456]

di1 = p (some integer) "123"
--- [123]

di2 = p (some integer) "123 456"
-- [123, 456]

di3 = p (some integer) "123\n\n 456"
-- [123, 456]

p' :: Parser [Integer]
p' = some $ do
    i <- token (some digit)
    return (read i)

tknWhole = token $ char 'a' >> char 'b'
tknCharA = (token (char 'a')) >> char 'b'

b1 = p tknWhole "a b"
-- error
b1' = p tknCharA "a b"
-- "b"

b2 = p tknWhole "ab ab"
-- "b"
b2' = p (some tknCharA) "a ba b"
-- "bb"

b3 = p (some tknWhole) "ab ab"
-- "bb"
b3' = p (some tknCharA) "a b a b"
-- "b"
-- stops at the first "a b" parse, because the parser doesn't say anything
-- about a space after 'b', and the tokenization behavior only applies to what 
-- follows 'a'.

tokenA = token (char 'a')
tokenB = token (char 'b')
tokenBoth = tokenA >> tokenB

b3'' = p (some tokenBoth) "a b a b"
-- "bb"

-- !! tokenrization isn't exclusively about whitespace; it's about ignoring noise,
-- so you can focus on the structures you are parsing.
