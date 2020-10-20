ifEven func x =
    if even x
        then func x
        else x

genIfEven f = (\x -> ifEven f x)

flipBinaryArgs f = (\x y -> f y x)

-- Q5.2
binaryPartialApp f = (\x -> (\y -> f x y))