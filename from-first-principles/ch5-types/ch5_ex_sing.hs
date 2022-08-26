module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if x > y then fstString x else sndString y
    where
        x = "Sing in"
        y = "Somewhere"

singOther :: String -> String
singOther song = if x > song then fstString x
                 else
                     if y > song then sndString y
                     else song
    where
        x = "Sing in"
        y = "Somewhere"

