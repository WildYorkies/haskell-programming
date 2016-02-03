-- chap_exercises.hs
module Chap_exercises where

stops  = "pbtdkg"
vowels = "aeiou"

-- Write a function that takes inputs from stops and vowels and makes 3-tuples of all possible stop-vowel-stop combinations. These will not all correspond to real words in English, although the stop-vowel-stop pattern is common enough that many of them will.
wordMaker :: [Char] -> [Char] -> [(Char, Char, Char)]
wordMaker stops vowels = [(s,v,s') | s <- stops, v <- vowels, s' <- stops]

-- Modify that function so that it only returns the combinations that begin with a p.
wordMakerP :: [Char] -> [Char] -> [(Char, Char, Char)]
wordMakerP stops vowels = filter (\(x,y,z) -> x == 'p') [(s,v,s') | s <- stops, v <- vowels, s' <- stops]

--Now set up lists of nouns and verbs (instead of stops and vowels) and modify the function to make tuples representing possible noun-verb-noun sentences.
nouns = ["dog", "cat", "human", "monkey", "elephant"]
verbs = ["runs", "bites", "jumps", "kisses", "hugs"]

sentenceMaker :: [String] -> [String] -> [(String, String, String)]
sentenceMaker nouns verbs = [(n,v,n') | n <- nouns, v <- verbs, n' <- nouns]

-- figured out what this anon func did and converted it to Fractional division instead of Integral
seekritFunc x = fromIntegral total / fromIntegral numWords where
  total = (sum (map length (words x))) 
  numWords = (length (words x))

-- rewriting functions with fold

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
--myAny f (x:xs) = if f x == True then True else myAny f xs
--myAny f (x:xs) = f x || myAny f xs
--myAny f = foldr (\a b -> if f a == True then True else b) False
myAny f = foldr (\a b -> f a || b) False 

myElem :: Eq a => a -> [a] -> Bool
--myElem n (x:xs) = n == x || myElem n xs
--myElem n = foldr (\a b -> a == n || b) False
myElem n = any (\x -> x == n)

myReverse :: [a] -> [a]
--myReverse [] = []
--myReverse (x:xs) = myReverse xs ++ [x]
myReverse l = foldl (flip (:)) [] l

myMap :: (a -> b) -> [a] -> [b]
myMap f l = foldr (\a b -> f a : b) [] l
-- myMap f = foldr ((:) . f) []

mapInts :: (Int -> Bool) -> [Int] -> [Bool]
mapInts f l = foldr (\a b -> f a : b) [] l

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f l = foldr (\a b -> if f a then a : b else b) [] l

squish :: [[a]] -> [a]
squish ll = foldr (\a b -> a ++ b) [] ll
-- squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f l = foldr (\a b -> f a ++ b) [] l
-- squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain ll = squishMap (\a -> a) ll

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f l = foldr (\a b -> case f a b of GT -> a; _ -> b) (head l) l

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f l = foldr (\a b -> case f a b of LT -> a; _ -> b) (head l) l

