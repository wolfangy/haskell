type FirstName = String
type LastName = String
type MiddleName = String

data Author = Author Name
instance Show Author where
    show (Author name) = "Author " ++ (show name)

data Artist = Person Name | Band String
instance Show Artist where
    show (Person name) = "Artist: " ++ (show name)
    show (Band band) = "Artist Band: " ++ band

data Creator = AuthorCreator Author | ArtistCreator Artist
instance Show Creator where
    show (AuthorCreator c) = show c
    show (ArtistCreator c) = show c

data Name = Name FirstName LastName
    | NameWithMiddle FirstName MiddleName LastName
    | TwoInitialsWithLast Char Char LastName

instance Show Name where
    show (Name firstName lastName) = firstName ++ "."++lastName
    show (NameWithMiddle firstName middleName lastName) = firstName ++ " " ++ middleName ++ " " ++ lastName
    show (TwoInitialsWithLast f m lastName) = [f] ++ " " ++ [m] ++ " " ++ lastName

data Book = Book {
    author :: Creator
    , isbn :: String
    , bookTitle :: String
    , bookYear :: Int
    , bookPrice :: Double
}

data VinylRecord = VinylRecord {
    artist :: Creator
    , recordTitle :: String
    , recordYear :: Int
    , recordPrice :: Double
}

data CollectibleToy = CollectibleToy {
    name :: String
    , description :: String
    , toyPrice :: Double
}

data StoreItem = BookItem Book | RecordItem VinylRecord | ToyItem CollectibleToy | FreePamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (FreePamphletItem _) = 0.0

-- Q16.1

data Pamphlet = Pamphlet {
    title :: String
    , pampletDescription :: String
    , contact :: Organization
}

data Organization = Organization {
    orgName :: String
    , address :: String
}

pamphlets = FreePamphletItem (
    Pamphlet { 
        title = "How to become rich in 21 days",
        pampletDescription = "A secret of rich people",
        contact = Organization { orgName = "Not-A-Fraud", address="You will not find my address"}
    })


-- Q16.2
class Enclosed a where
    perimeter :: a -> Double

data Shape = Circle Double | Square Double | Rectangle Double Double

instance Enclosed Shape where
    perimeter (Circle r) =  2 * pi * r
    perimeter (Square l) = 4 * l
    perimeter (Rectangle l w) = 2 * l + 2 * w

c1 = Circle 2
c2 = Circle 4

s1 = Square 2
s2 = Square 4

rect1 = Rectangle 4 3
rect2 = Rectangle 16 9