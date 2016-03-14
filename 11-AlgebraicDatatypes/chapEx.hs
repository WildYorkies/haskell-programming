-- chapEx.hs
module ChapEx where

import Data.Char (toUpper)

-- as-patterns

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf (x:xs) ys@(y:_) = elem x ys && isSubsequenceOf xs ys -- how to us as-pattern here?

capitalizeWords :: String -> [(String, String)]
capitalizeWords sentence = map (\word@(w:ws) -> (word, (toUpper w) : ws) ) $ words sentence

-- other stuff

capitalizeWord :: String -> String
capitalizeWord (w:ws) = toUpper w : ws

capitalizeParagraph :: String -> String
capitalizeParagraph para =  stitch . map capitalizeWord . sentences $ para
  where stitch :: [String] -> String 
        stitch l = foldr (\a b -> a ++ ". " ++ b) [] l

sentences :: String -> [String]
sentences []   = []
sentences para = firstSen : sentences otherSens
  where firstSen = takeWhile (/= '.') para 
        otherSens = dropWhile (== ' ') . dropWhile (== '.') . dropWhile (/= '.') $ para





