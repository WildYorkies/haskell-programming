-- wordNumber.hs
module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
  | n == 9 = "nine"
  | n == 8 = "eight"
  | n == 7 = "seven"
  | n == 6 = "six"
  | n == 5 = "five"
  | n == 4 = "four"
  | n == 3 = "three"
  | n == 2 = "two"
  | n == 1 = "one"
  | n == 0 = "zero"
  | otherwise = "NA"

digits :: Int -> [Int]
digits 0 = []
digits n = digits (div n 10) ++ [mod n 10]

wordNumber :: Int -> String
wordNumber n = concat (intersperse "-" listOfNums)
  where listOfNums = map digitToWord (digits n)
