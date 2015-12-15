-- ex.hs
module Ex where

-- Recursion #2
sumTo :: (Eq a, Num a) => a -> a
sumTo 0 = 0
sumTo n = n + sumTo (n - 1)

-- Recursion #3
myMult :: (Eq a, Num a) => a -> a -> a
myMult x y = go x y 0 0
  where go num timesBy total count
         | count == timesBy = total
         | otherwise        = go num timesBy (total + num) (count + 1)
