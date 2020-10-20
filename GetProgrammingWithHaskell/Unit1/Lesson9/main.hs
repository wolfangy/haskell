myMap f [] = []
myMap f (x:xs) = (f x) : myMap f xs

myFilter f [] = []
myFilter f (x:xs) = 
    if f x
        then x:myFilter f xs
        else myFilter f xs

myFoldl f acc [] = acc
myFoldl f acc (x:xs) =
    myFoldl f (f acc x) xs

myFoldr f acc [] = acc
myFoldr f acc (x:xs) = f x rightResult
    where rightResult = myFoldr f acc xs

myProduct = myFoldl (*) 1

rcons x y = y:x
myReverse aList = myFoldl rcons [] aList

myElem item aList =
    length (myFilter (== item) aList) > 0

harmonic n =
    myFoldr (+) 0 (map ((/) 1.0) [1..n])