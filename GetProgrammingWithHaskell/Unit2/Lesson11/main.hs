int6 = read "6" :: Int
doubleVal = read "6" :: Double

ifEven :: (Int -> Int) -> Int -> Int
ifEven f n = if even n
             then f n
             else n

simple :: a -> a
simple x = x

makeTriple :: a -> b -> c -> (a,b,c)
makeTriple x y z = (x,y,z)


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs) =
    if f x
        then x: myFilter f xs
        else myFilter f xs

-- myHead :: [a] -> [a0]
-- myHead [] = []
-- myHead (x:xs) = x
myTail :: [a] -> [a]
myTail [] =[]

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x