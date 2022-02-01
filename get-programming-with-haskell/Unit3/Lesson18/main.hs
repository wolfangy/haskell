import Data.Map

data Box a = Box a deriving Show

testInt = 6 :: Int
testBoxInt = Box testInt

testWord = "box"
testBoxWord = Box testWord


testFn = (\x -> x)
testBoxFn = Box testFn

testF x = x
testBoxF = Box testF

testBoxBox = Box testBoxInt

wrap :: a -> Box a
wrap v = (Box v)

unwrap :: Box a -> a
unwrap (Box v) = v


data Tripple a = Tripple a a a deriving Show


type Point3D = Tripple Double

aPoint :: Point3D
aPoint = Tripple 0.1 0.53 0.12


type FullName = Tripple String

aPerson :: FullName
aPerson = Tripple "Howard" "Phillips" "Lovecraft"

type Initials = Tripple Char
testInitials :: Initials
testInitials = Tripple 'H' 'P' 'L'

first :: Tripple a -> a
first (Tripple f _ _) = f

second :: Tripple a -> a
second (Tripple _ s _) = s

third :: Tripple a -> a
third (Tripple _ _ t) = t

toList :: Tripple a -> [a]
toList (Tripple x y z) = [x, y, z]

transform :: (a -> a) -> Tripple a -> Tripple a
transform f (Tripple x y z) = Tripple (f x) (f y) (f z)


----------------------------------------------------------

data List a = Empty | Cons a (List a) deriving Show

listExp1 = Cons 1 (Cons 2 (Cons 3 Empty))

myMap :: (a -> b) -> List a -> List b
myMap _ Empty = Empty
myMap f (Cons x xs) = Cons (f x) (myMap f xs)


--------------------------------------------------------------


data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]


organPairs :: [(Int, Organ)]
organPairs = zip ids organs

-- fromList :: Ord k => [(k, a)] -> Map k a

organCatalog :: Map Int Organ
organCatalog = fromList organPairs

-- Map.lookup :: Ord k => k -> Map.Map k a -> Maybe a
lookupHeart = Data.Map.lookup 7 organCatalog


-- Q18.1
trippleMap :: (a -> b) -> Tripple a -> Tripple b
trippleMap fn (Tripple x y z) = Tripple (fn x) (fn y) (fn z)

boxMap :: (a -> b) -> Box a -> Box b
boxMap fn (Box v) = Box (fn v)

-- Q18.2

instance Ord Organ where
    compare l r = toOrd sub
        where
            sub = (-) (fromEnum l) (fromEnum r)
            toOrd = (\ val -> case () of
              _ | val == 0 -> EQ
                | val < 0 -> LT
                | otherwise -> GT)


values :: [Organ]
values = Prelude.map snd (Data.Map.toList organCatalog)

allOrgans :: [Organ]
allOrgans = [Heart .. Spleen]
allOrgans' = Prelude.foldl (\ acc cur -> if elem cur acc == False then cur:acc else acc) [] values

organCounts :: [Int]
organCounts = Prelude.map countOrgan allOrgans
    where 
        countOrgan = (\organ -> (length . (Prelude.filter (== organ))) values)

organInventory :: Map Organ Int
organInventory = fromList (zip allOrgans organCounts)