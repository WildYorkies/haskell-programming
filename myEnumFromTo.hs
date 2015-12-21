-- myEnumFromTo.hs
module MyEnumFromTo where

myEnumFromTo :: (Enum a, Ord a) => a -> a -> [a]
myEnumFromTo start stop = go start stop []
  where go x y myRange
         | x > y     = reverse myRange
         | otherwise = go (succ x) y (x : myRange)