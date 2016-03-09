-- intermission.hs
{-# LANGUAGE FlexibleInstances #-}

data PugType = PugData
-- Type constant that enumerates one constructor. and constant value

data HuskyType a = HuskyData
-- Type constructor that enumerates one data constructor. Type argument is phantom because it has no witness. Constant value as well

data DogueDeBordeaux doge = DogueDeBordeaux doge
-- type constructor enumerates one constructor. data constructor.

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)

-- Exercises
data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)
data Airline =
  PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)
data Manufacturer =
  Mini
  | Mazda
  | Tata
  deriving (Eq, Show)
data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 700)
doge = Plane PapuAir (Size 5)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True 
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _         = False

areCars :: [Vehicle] -> [Bool]
areCars = foldr (\a b -> case a of (Car _ _) -> True : b; otherwise -> False : b) [] 

getManu :: Vehicle -> Maybe Manufacturer
getManu (Car m p) = Just m
getManu _         = Nothing

class TooMany a where
  tooMany :: a -> Bool

instance TooMany (Int, String) where
  tooMany (n,s) = n < 10
instance TooMany (Int, Int) where
  tooMany (n, n') = (n + n') > 42
instance (Num a, TooMany a) => TooMany (a,a) where
  tooMany (n, n') = True
