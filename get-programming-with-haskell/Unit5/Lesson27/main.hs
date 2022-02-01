import qualified Data.Map as Map

printInt :: Maybe String -> IO()
printInt Nothing = putStrLn "value missing"
printInt (Just val) = putStrLn val

-- printInt $ fmap show (Just "4")

successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just n) = Just (n + 1)
incMaybe Nothing = Nothing

incMaybe' :: Maybe Int -> Maybe Int
incMaybe' m = (+ 1) <$> m

successStr :: Maybe String
successStr = show <$> successfulRequest

failStr :: Maybe String
failStr = show <$> failedRequest

-- Q1
reverseMaybe :: Maybe String -> Maybe String
reverseMaybe m = reverse <$> m

data RobotPart = RobotPart {
    name :: String
    , description :: String
    , cost :: Double
    , count :: Int
} deriving Show

leftArm :: RobotPart
leftArm = RobotPart {
    name = "left arm"
    , description = "left arm for face punching"
    , cost = 1000.0
    , count = 3

}

rightArm :: RobotPart
rightArm = RobotPart {
    name = "right arm"
    , description = "right arm for kind hand gestures"
    , cost = 1025.0
    , count = 5

}

robotHead :: RobotPart
robotHead  = RobotPart
   { name = "robot head"
   , description = "this head looks mad"
   , cost = 5092.25
   , count = 2
   }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat ["<h2>",partName, "</h2>"
                          ,"<p><h3>desc</h3>",partDesc
                          ,"</p><p><h3>cost</h3>"
                          ,partCost
                          ,"</p><p><h3>count</h3>"
                          ,partCount,"</p>"]
    where
        partName = name part
        partDesc = description part
        partCost = show (cost part)
        partCount = show (count part)

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
    where
        keys = [1, 2, 3]
        vals = [leftArm, rightArm, robotHead]
        keyVals = zip keys vals

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = snd <$> (Map.toList partsDB)

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

-- Q27.1
data Box a = Box a deriving Show

instance Functor Box where
    fmap fn (Box x) = Box (fn x)


morePresent :: Int -> Box a -> Box [a]
morePresent n box = (takeNRepeat n) <$> box
    where takeNRepeat num = (take num) . repeat

-- Q27.2
myBox :: Box Int
myBox = Box 1

wrap :: a -> Box a
wrap item = Box item

wrapped = fmap wrap myBox

unwrap :: Box a -> a
unwrap (Box item) = item

unwrapped = fmap unwrap wrapped

-- Q27.3
showCost :: RobotPart -> String
showCost part = (show . cost) part

lookupPart :: Int -> Maybe RobotPart
lookupPart id = Map.lookup id partsDB

queryCost :: Int -> Maybe String
queryCost id = showCost <$> (lookupPart id)