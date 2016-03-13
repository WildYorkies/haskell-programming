-- vigenereCipher.hs
module VigenereCipher (vigenere) where

import Data.List (cycle)
import Data.Char (toUpper)
import Data.Maybe (fromJust)

data Operation = Hide | Reveal

findNum :: Char -> Int
findNum x = fromJust $ lookup x alphaList
  where alphaList :: [(Char, Int)]
        alphaList = zip ['A'..'Z'] [0..25]

findLetter :: Int -> Char
findLetter x | x > 25    = fromJust $ lookup (mod x 26) numList
             | x < 0     = fromJust $ lookup (length numList + x) numList
             | otherwise = fromJust $ lookup x numList
  where numList :: [(Int, Char)]
        numList = zip [0..25] ['A'..'Z']

createCipher :: String -> String -> [(Char, Char)]
createCipher phrase keyword = go (map toUpper phrase) (cycle (map toUpper keyword))
  where go []       (k:ks) = []
        go (' ':ps) (k:ks) = (' ', ' ') : go ps (k:ks)
        go (p:ps)   (k:ks) = (p, k)     : go ps ks

shiftChar :: Operation -> (Char, Char) -> Char
shiftChar _  (' ', ' ') = ' '
shiftChar Hide   (x, y) = findLetter ((findNum x) + (findNum y))
shiftChar Reveal (x, y) = findLetter ((findNum x) - (findNum y))

vigenere :: String -> String -> Operation -> String
vigenere phrase keyword op = map (shiftChar op) $ createCipher phrase keyword