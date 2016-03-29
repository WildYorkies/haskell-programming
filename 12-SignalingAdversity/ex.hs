-- ex.hs
module Ex where

import Data.List (intercalate)
import Data.Maybe (fromJust)

-- Write a recursive function that takes a text/string, 
-- breaks it into words and replaces each instance of ”the” with ”a”. 
-- It’s intended only to replace exactly the word “the”.

notThe :: String -> Maybe String
notThe w
  | w == "the" = Nothing
  | otherwise  = Just w

replaceThe :: String -> String
replaceThe sentence = intercalate " " (replace (words sentence))
  where replace :: [String] -> [String]
        replace    []  = []
        replace (w:ws) = case notThe w of Nothing -> "a" : replace ws 
                                          Just x  ->  x  : replace ws 

-- Write a recursive function that takes a text/string, breaks it into words, 
-- and counts the number of instances of ”the” followed by a vowel-initial word.

-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow" 
-- 1
vowels :: [Char]
vowels = ['a','e','i','o','u']

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel sentence = count (words sentence) 0
  where count :: [String] -> Integer -> Integer 
        count     []    inc                         = inc
        count (w:[])    inc                         = inc
        count (w:w':ws) inc
          | (w == "the") && (elem (head w') vowels) = count ws (inc + 1)
          | otherwise                               = count ws inc 

-- Return the number of letters that are vowels in a word.
-- Hint: it’s helpful to break this into steps. 
-- Add any helper func- tions necessary to achieve your objectives.
countVowels :: String -> Integer 
countVowels s = count s 0
  where count :: String -> Integer -> Integer
        count   []   inc = inc 
        count (x:xs) inc = case elem x vowels of True -> count xs (inc + 1)
                                                 False -> count xs inc 

-- Use the Maybe type to write a function that counts the number of vowels in a string and 
-- the number of consonants. 
-- If the number of vowels exceeds the number of consonants, the function returns Nothing.
newtype Word' =
  Word' String 
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord w = if vowels > consonants then Nothing else Just (Word' w)
  where vowels = countVowels w
        consonants = countConsonants w

countConsonants :: String -> Integer 
countConsonants s = count s 0
  where count :: String -> Integer -> Integer
        count   []   inc = inc 
        count (x:xs) inc = case elem x vowels of True -> count xs inc
                                                 False -> count xs (inc + 1)

-- Convert between Natural nums and Integers
data Nat =
  Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ x) = 1 + natToInteger x 

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0     = Nothing
  | x == 0    = Just Zero
  | otherwise = Just (Succ (fromJust (integerToNat (x - 1))))

-- UNFOLDS

myIterate :: (a -> a) -> a -> [a]
myIterate f acc = acc : myIterate f (f acc)

-- take 10 $ myUnfoldr (\b -> Just (b, b+1)) 0
-- [0,1,2,3,4,5,6,7,8,9]
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f acc = case f acc of Nothing      -> []
                                Just (x, x') -> x : myUnfoldr f x'


betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x 

data BinaryTree a = 
  Leaf
  | Node (BinaryTree a) a (BinaryTree a) 
  deriving (Eq, Ord, Show)

-- unfold (\a -> Just (Node x y z, a+1, Node q r s)) 0
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b 
unfold f acc = case f acc of Nothing             -> Leaf
                             Just (left,n,right) -> Node (unfold f left) n (unfold f right)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\a -> if a < n then Just (a+1, a, a+1) else Nothing) 0

