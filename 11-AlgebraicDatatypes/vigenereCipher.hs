-- vigenereCipher.hs
module VigenereCipher where

import Data.List (cycle)
import Data.Char (ord, chr, toUpper, isAlpha)
import Data.Maybe (fromJust)

phrase = "MEET AT DAWN"
keyword = "ALLY"
cipherTest = "MPPR AE OYWY"

alphaList :: [(Char, Int)]
alphaList = zip ['A'..'Z'] [0..25]

numList :: [(Int, Char)]
numList = zip [0..25] ['A'..'Z']

findNum :: Char -> Int
findNum x = fromJust $ lookup x alphaList

findLetter :: Int -> Char
findLetter x | x > 25    = fromJust $ lookup (mod x 26) numList
             | x < 0     = fromJust $ lookup (length numList + x) numList
             | otherwise = fromJust $ lookup x numList 

cipherPhrase :: String -> String -> [(Char, Char)]
cipherPhrase secret keyword = go (map toUpper secret) (cycle (map toUpper keyword))
  where go []       (k:ks) = []
        go (' ':ps) (k:ks) = (' ', ' ') : go ps (k:ks)
        go (p:ps)   (k:ks) = (p, k)     : go ps ks

shiftChar :: (Char, Char) -> Char
shiftChar (' ', ' ') = ' '
shiftChar (x, y) = findLetter ((findNum x) + (findNum y))

unShiftChar :: (Char, Char) -> Char
unShiftChar (' ', ' ') = ' '
unShiftChar (x, y) = findLetter ((findNum x) - (findNum y))

vigenere :: String -> String -> String
vigenere secret keyword = map shiftChar $ cipherPhrase secret keyword

unVigenere :: String -> String -> String
unVigenere scramble keyword = map unShiftChar $ cipherPhrase scramble keyword