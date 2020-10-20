data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
    where
        halfAlphabet = alphabetSize `div` 2
        offset = fromEnum c + halfAlphabet
        rotation = offset `mod` alphabetSize

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
    where 
        halfN = n `div` 2
        offset = if even n 
            then fromEnum c + halfN
            else 1 + fromEnum c + halfN
        rotation = offset `mod` n

rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
    where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder vals = map rot4l vals
    where
        alphabetSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
        rot4l = rotN alphabetSize

data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Show, Enum, Bounded)

threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals = map rot3l vals
    where
        alphabetSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
        rot3l = rotN alphabetSize

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals = map decode vals
    where
        decode = rotNdecoder 3


xorBool :: Bool -> Bool -> Bool
-- xorBool True True = False
-- xorBool False False = False
-- xorBool _ _ = True
xorBool l r = (l || r) && (not (l && r))

xorPair :: (Bool, Bool) -> Bool
xorPair (l, r) = xorBool l r

xor :: [Bool] -> [Bool] -> [Bool]
-- xor [] [] = []
-- xor (lx:lxs) (rx:rxs) = (xorPair (lx, rx)) : (xor lxs rxs)
xor l r = map xorPair (zip l r)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
    if (remainder == 0)
        then False : intToBits' nextVal
        else True : intToBits' nextVal
    where
        remainder = n `mod` 2
        nextVal = n `div` 2

maxBits = length (intToBits' (maxBound :: Int))

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
    where
        reversedBits = reverse (intToBits' n)
        missingBits = maxBits - (length reversedBits)
        leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits ch = intToBits (fromEnum ch)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\ x -> 2 ^ (snd x)) trueLocations)
    where
        size = length bits
        indices = [size-1, size-2 .. 0]
        trueLocations = filter (\ x -> fst x == True) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"


applyOTP' :: String -> String -> [Bits]
applyOTP' pad plainText = 
    map (\pair -> (fst pair) `xor` (snd pair))
    (zip padBits plainTextBits)
    where
        padBits = map charToBits pad
        plainTextBits = map charToBits plainText

applyOTP :: String -> String -> String
applyOTP pad text = map bitsToChar bitList
    where bitList = applyOTP' pad text

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

class Cipher a where
    encode :: a -> String -> String
    decode :: a -> String -> String


data Rot = Rot

-- instance Cipher Rot where
--     encode Rot text = rotEncoder text
--     decode Rot text = rotDecoder text

data OneTimePad = OTP String

instance Cipher OneTimePad where
    encode (OTP pad) text = applyOTP pad text
    decode (OTP pad) text = applyOTP pad text


myOTP :: OneTimePad
myOTP = OTP (cycle [minBound :: Char .. maxBound :: Char])