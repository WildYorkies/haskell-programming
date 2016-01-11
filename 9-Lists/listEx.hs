-- listEx.hs
import Data.Char

keepUppers :: [Char] -> [Char]
keepUppers = filter isUpper

capItUp :: [Char] -> [Char]
capItUp "woot" = "WOOT"
capItUp (x:xs) = toUpper x : xs

capAndReturn :: [Char] -> Char
capAndReturn = toUpper . head

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs
--            if x == True then True else myOr xs 

myAny :: (a -> Bool) -> [a] -> Bool
myAny f []     = False
myAny f (x:xs) = f x || myAny f xs
--               if f x == True then True else myAny f xs 

myElem :: Eq a => a -> [a] -> Bool
myElem y []     = False
myElem y (x:xs) = if y == x then True else myElem y xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse l = myReverse (tail l) ++ [head l]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = f x ++ squishMap f xs 

myMaximum :: Ord a => [a] -> a
myMaximum (x:x':[]) = 
  case compare x x' of GT -> x
                       LT -> x'
                       EQ -> x'
myMaximum (x:x':xs) = 
  case compare x x' of GT -> myMaximum (x:xs)
                       LT -> myMaximum (x':xs)
                       EQ -> myMaximum (x':xs)

myMinimum :: Ord a => [a] -> a
myMinimum (x:x':[]) = 
  case compare x x' of GT -> x'
                       LT -> x
                       EQ -> x'
myMinimum (x:x':xs) = 
  case compare x x' of GT -> myMinimum (x':xs)
                       LT -> myMinimum (x:xs)
                       EQ -> myMinimum (x':xs)





