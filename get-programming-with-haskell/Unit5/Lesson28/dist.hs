import qualified Data.Map as Map

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [("Arkham",(42.6054,-70.7829))
                          ,("Innsmouth",(42.8250,-70.8150))
                          ,("Carcosa",(29.9714,-90.7694))
                          ,("New York",(40.7776,-73.9691))]

toRadius :: Double -> Double
toRadius degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double, Double)
latLongToRads (lat, long) = (rlat, rlong)
    where
        rlat = toRadius lat
        rlong = toRadius long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
    where
        (rlat1, rlong1) = latLongToRads coords1
        (rlat2, rlong2) = latLongToRads coords2
        dlat = rlat2 - rlat1
        dlong = rlong2 - rlong1
        a = (sin (dlat/2))^2 + cos rlat1 * cos rlat2 * (sin (dlong/2))^2
        c = 2 * atan2 (sqrt a) (sqrt (1-a))
        earthRadius = 3961.0

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")

maybeInc = (+) <$> Just 1

startCity = Map.lookup "Carcosa" locationDB
destCity = Map.lookup "Innsmouth" locationDB

calculatedDist :: Maybe Double
calculatedDist = haversine <$> startCity <*> destCity

main :: IO ()
main = do
    putStrLn "Enter the starting city name:"
    startingInput <- getLine
    let startingCity = Map.lookup startingInput locationDB
    putStrLn "enter the destination city name:"
    destinationInput <- getLine
    let destinationCity = Map.lookup destinationInput locationDB
    let distance = haversine <$> startingCity <*> destinationCity
    printDistance distance

-- Q28.1
haversineIO' :: IO LatLong -> IO LatLong -> IO Double
haversineIO' coords1 coords2 = do
    l <- coords1
    r <- coords2
    let d = haversine l r
    return d

-- Q28.2
haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO coords1 coords2 = haversine <$> coords1 <*> coords2

readLatLong :: IO LatLong
readLatLong = do
    l <- getLine
    let ld = read l :: Double
    r <- getLine
    let rd = read r :: Double
    return (ld, rd)