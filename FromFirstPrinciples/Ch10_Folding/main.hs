module Ch10 where

xs = map show [1..10]

yr = foldr (\x y -> concat ["(", x, "+", y, ")"]) "0" xs

yl = foldl (\x y -> concat ["(", x, "+", y, ")"]) "0" xs
