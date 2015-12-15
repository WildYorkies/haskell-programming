-- exercises.hs
module Exercises where

-- 1
tensDigit :: Integral a => a -> a
tensDigit x = fst (divMod x 10)

hunsD :: Integral a => a -> a
hunsD x = fst (divMod x 100)

-- 2
foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y myBool =
  case myBool == True of
    y -> x
    x -> y

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y myBool
  | myBool == True  = x
  | myBool == False = y

-- 3
g :: (a -> b) -> (a, c) -> (b, c)
g f x = (y, z)
  where y = f (fst x)
        z = snd x

-- 4
roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 = 





