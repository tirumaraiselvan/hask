import qualified Data.Map as Map

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r * r
surface (Rectangle (Point x1 y1) (Point x2 y2) ) = (abs $ x2 - x1) * (abs $ y2 - y1)


data Person = Person {
    firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String,
    flavor :: String
} deriving (Eq, Show, Read)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t

(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)


type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnum pbook = (name, pnum) `elem` pbook


data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)


lockerLookup :: Int -> LockerMap -> Either String Code

lockerLookup lockNum lockMap =
    case Map.lookup lockNum lockMap of
        Nothing -> Left $ "Locker num" ++ show lockNum ++ " doesn't exist"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockNum ++ " is already taken"



lockers = Map.fromList [(100,(Taken,"ZD39I")), (101,(Free,"JAH3I")), (103,(Free,"IQSA9")), (105,(Free,"QOTSA")), (109,(Taken,"893JJ")), (110,(Taken,"99292")) ]
