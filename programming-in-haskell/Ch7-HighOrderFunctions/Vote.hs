module Vote where

import Data.List

{- 
Every person has one vote,
and the candidate with the largest number
of votes is declared the winner.
-}

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- to remove duplicate items from a list:

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : filter (/= x) (rmdups xs)

rmdups' :: Eq a => [a] -> [a]
rmdups' = foldr addToSet []
    where 
      addToSet n [] = [n]
      addToSet n xss@(x: xs)
              | n == x = xss
              | otherwise = addToSet n xs ++ [x]

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

-- pairs are ordered lexicographically


winner :: Ord a => [a] -> a
winner = snd . last . result

{-
Alternative vote
-}

ballots :: [[String]]
ballots =
  [ ["Red", "Green"],
    ["Blue"],
    ["Green", "Red", "Blue"],
    ["Blue", "Green", "Red"],
    ["Green"]
  ]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/=[])

elim :: Eq a => a -> [[a]] -> [[a]]
elim a = map (filter (/=a))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
                [c] -> c
                (c:cs) -> winner' (elim c bs)