-- palindrome.hs
module Palindrome where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x =
  if x == rvrsd then True else False
  where rvrsd = reverse x
