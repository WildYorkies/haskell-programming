-- intDiv.hs
module IntDiv where

data DividedResult =
    Result (Integer, Integer)
  | DividedByZero
      deriving (Show)

dividedBy :: Integer -> Integer -> DividedResult
dividedBy num denom
  | denom == 0                = DividedByZero
  | (num < 0) && (denom < 0 ) = Result (go (negate num) (negate denom) 0)
  | (num < 0)                 = Result (go (negate num) denom 0)
  | (denom < 0)               = Result (go num (negate denom) 0)
    where go n   d count
           | n < d     = (count, n)
           | otherwise = go (n - d) d (count + 1)
