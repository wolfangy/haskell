import qualified Data.Map as Map
import Data.Maybe
import Data.List

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- data Maybe a = Nothing | Just a

lookupResult1= Map.lookup 13 organCatalog
lookupResult2 = Map.lookup 6 organCatalog


possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
    where getContents = \ id -> Map.lookup id catalog

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ availables = (length . (filter (\o -> o == Just organ))) availables

-- isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans
justTheOrgans' = filter isJust availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""

newtype MaybeOrgan = MaybeOrgan (Maybe Organ)

instance Show MaybeOrgan where
    show (MaybeOrgan maybeOrgan) = showOrgan maybeOrgan

organList :: [String]
organList = map showOrgan justTheOrgans
organList' = ((map show) . (map (\x -> MaybeOrgan x))) justTheOrgans

cleanList :: String
cleanList = intercalate ", " organList

numOrZero :: Maybe Int -> Int
numOrZero Nothing = 0
numOrZero (Just x) = x

-------------------------------------------------------------------------------

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
    show (Vat organ) = show organ ++ " in a vat"
    show (Cooler organ) = show organ ++ " in a cooler"
    show (Bag organ) = show organ ++ " int a bag"

data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ


placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

report :: (Location, Container) -> String
report (location, container) = show container ++ " in the " ++ show location

processed = map process [Brain, Heart, Spleen, Kidney]
reported = map report processed

processAndReport :: (Maybe Organ) -> String
processAndReport (Just organ) = report (process organ)
processAndReport Nothing = "error, id not found"

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
    where 
        organ = Map.lookup id catalog


-- Q19.1
emptyDrawers :: [Maybe Organ] -> [Bool]
emptyDrawers drawers = map
    (\x -> case x of
        (Just organ) -> False
        Nothing -> True) drawers

-- Q19.2
maybeMap :: (a -> b) -> [(Maybe a)] -> [(Maybe b)]  
maybeMap _ [] = []
maybeMap fn (x:xs) = maybeX : (maybeMap fn xs)
    where
        map' (Just val) = Just (fn val)
        map' Nothing = Nothing
        maybeX = map' x

maybeMapV2 :: (Maybe a) => (a -> b) -> a -> b
maybeMapV2 _ [] = []
