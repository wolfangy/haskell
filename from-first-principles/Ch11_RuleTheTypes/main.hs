{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ch11_RuleTheTypes where

-- 11.7
--
data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

type Width = Integer 
type Length = Integer
type Height = Integer

data Size = Size Length Width Height deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 12000)
clownCar = Car Tata (Price 2000)
doge = Plane PapuAir

-- 1. myCar :: Vehicle
--

-- 2.
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar


-- 3.
-- 
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _ = undefined

-- 4.
--

-- 5.
-- 
myPlane = Plane CatapultsR'Us (Size 300 180 100)

-- 11.8
--

-- 3. 2 ^ 16 = 65,536
--

-- 4. 
--

intLowBound = minBound :: Int
intHighBound = maxBound :: Int

-- 5. 2^8 = 256
--

data Example = MakeExample deriving Show

data Exmaple' = MakeExample' Int deriving Show

-- newtype
--
class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 1024

newtype Goats = Goats Int deriving (Eq, Ord, Show)
newtype Cows = Cows Int deriving (Eq, Ord, Show)

instance TooMany Goats where
    tooMany (Goats g) = g > 43

instance TooMany Cows where
    tooMany (Cows c) = c > 18

newtype Goats' = Goats' Int deriving (Eq, Ord, Show, TooMany)
-- Goats' will reuse the Int instance for TooMany
--
--

data GuessWhat = Chickenbutt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a
            | Second b 
            deriving (Eq, Show)

data RecordProduct a b =
    RecordProduct {
        pfirst :: a
        , psecond :: b }
        deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig 

newtype NumSheep = NumSheep Int deriving (Eq, Show)

data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep 
                    deriving (Eq, Show)

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)


type Name = String
type Age = Int
type LoveMud = Bool

type PoundsOfWood = Int

data CowInfo =
    CowInfo Name Age deriving (Eq, Show)

data PigInfo =
    PigInfo Name Age LoveMud deriving (Eq, Show)

data SheepInfo =
    SheepInfo Name Age PoundsOfWood deriving (Eq, Show)

data Animal =
    Cow CowInfo
    | Pig PigInfo
    | Sheep SheepInfo
    deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

bess' = CowInfo "Bess" 4
bess = First bess'

elmer'' = SheepInfo "Elmer" 5 5
elmer' = Second elmer''
elmer = Second elmer'

baaa = SheepInfo "Baaaa" 5 5

data OperatingSystem = 
    GNUPlusLinux
    | OpenBSD
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang = 
    Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer {
        os :: OperatingSystem,
        lang :: ProgLang
        } deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer {
    os = Mac,
    lang = Haskell
    }

feelingWizardly :: Programmer
feelingWizardly = Programmer {
    os = GNUPlusLinux,
    lang = Agda
    }

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [
    GNUPlusLinux,
    OpenBSD,
    Mac,
    Windows
    ]

allLanguages :: [ProgLang]
allLanguages = [
    Haskell,
    Agda,
    Idris,
    PureScript
    ]

allProgrammers :: [Programmer]
allProgrammers = [Programmer o l| 
    o <- allOperatingSystems,
    l <- allLanguages]

data ThereYet = There Float Int Bool deriving (Eq, Show)

nope :: Float -> Int -> Bool -> ThereYet
nope = There

notYet :: Int -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yussss :: ThereYet
yussss = notQuite False

-- Deconstructing Values
--

newtype NickName = NickName String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType =
    DairyFarmer
    | WheatFarmer
    | SoybeanFarmer
    deriving Show

data Farmer =
    Farmer NickName Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec =
    FarmerRec {
        name :: NickName
        , acres :: Acres
        , farmerType :: FarmerType
        } deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
    case farmerType farmer of
        DairyFarmer -> True
        _           -> False

-- The Quad
--
data Quad =
    One
    | Two
    | Three
    | Four
    deriving (Eq, Show)

-- 1.
eQuad :: Either Quad Quad
-- 4 + 4 = 8 inhabitants
eQuad = Left One
eQuad' = Left Two

-- 2.
-- 4 x 4 = 16 inhabitants
proQuad :: (Quad, Quad)
proQuad = (One, Two)


-- 3.
-- 4 ^ 4 = 256
funcQuad :: Quad -> Quad
funcQuad n = n

-- 4.
-- 2 x 2 x 2 = 8
prodTBool :: (Bool, Bool, Bool)
prodTBool = (True, True, True)

-- 5.
-- 2 ^ (2 x 2) = 2 ^ 4 = 16
gTwo :: Bool -> Bool -> Bool
gTwo l r = undefined

-- 6.
-- 4^(2 * 4) = 4^8 = 65536 
fTwo :: Bool -> Quad -> Quad
fTwo = undefined


-- Binary Tree
--

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a  = Node (insert' b left) a right
    | b > a  = Node left a (insert' b right)

t1 = insert' 0 Leaf

t2 = insert' 3 t1

t3 = insert' 5 t2

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
        if mapTree (+ 1) testTree' == mapExpected
        then print "yup okay!"
        else error "test failed!"

-- Convert binary tree to lists
--
preorder :: BinaryTree a -> [a]
preorder tree = go tree []
    where
        go Leaf arr = arr
        go (Node left a right) arr = a:arr ++ (go left arr) ++ (go right arr) 

preorder' Leaf = []
preorder' (Node left a right) = [a] ++ preorder' left ++ preorder' right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right


postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO()
testPreorder =
    if preorder testTree == [2,1,3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
    if inorder testTree == [1,2,3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad new bears."

testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1,3,2]
    then putStrLn "Postorder fine!"
    else putStrLn "Bad new bears."

main :: IO ()
main = do
    testPreorder
    testInorder
    testPostorder
