import Text.Read (readMaybe)

type Name = String
type Phone = String
type Location = String
type PhoneNumbers = [(Name, Phone)]
type Locations = [(Phone, Location)]

doubleStrNum :: (Num a , Read a) => String -> Maybe a
doubleStrNum str = (2*) <$> readMaybe str

plusStrNum :: (Num a, Read a) => String -> String -> Maybe a
plusStrNum s1 s2 = (+) <$> readMaybe s1 <*> readMaybe s2

locateByName :: PhoneNumbers ->  Locations -> Name -> Maybe Location
locateByName pns locs name = do
    pn <- lookup name pns
    lc <- lookup pn locs
    return lc

locateByName' :: PhoneNumbers -> Locations -> Name -> Maybe Location
locateByName' pns locs name = lookup name pns >>= flip lookup locs

phoneNumbers = [
    ("Alice", "1"),
    ("Bob", "2"),
    ("Cory", "3"),
    ("David", "4"),
    ("Eric", "5"),
    ("Fred", "6"),
    ("Gray", "7"),
    ("Harry", "8"), 
    ("Iry", "9"),
    ("Jim", "10")
    ]

locations = [
    ("1", "#1 Ave A"),
    ("2", "#2 Ave B"),
    ("3", "#3 Ave C"),
    ("4", "#4 Ave D"),
    ("5", "#5 Ave E"),
    ("6", "#6 Ave F"),
    ("7", "#7 Ave G"),
    ("8", "#8 Ave H"),
    ("9", "#9 Ave I"),
    ("10", "#10 Ave J")
    ]