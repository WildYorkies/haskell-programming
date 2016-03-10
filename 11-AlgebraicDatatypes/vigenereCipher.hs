-- vigenereCipher.hs
module VigenereCipher where

import Data.List (cycle)
import Data.Char (toUpper)
import Data.Maybe (fromJust)

phrase = "MEET AT DAWN"
keyword = "ALLY"
cipherTest = "MPPR AE OYWY"

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

cipherPhrase :: String -> String -> [(Char, Char)]
cipherPhrase secret keyword = go (map toUpper secret) (cycle (map toUpper keyword))
  where go []       (k:ks) = []
        go (' ':ps) (k:ks) = (' ', ' ') : go ps (k:ks)
        go (p:ps)   (k:ks) = (p, k)     : go ps ks

vigenere :: String -> String -> String
vigenere secret keyword = map shiftChar $ cipherPhrase secret keyword
  where shiftChar :: (Char, Char) -> Char
        shiftChar (' ', ' ') = ' '
        shiftChar (x, y)     = findLetter ((findNum x) + (findNum y))

unVigenere :: String -> String -> String
unVigenere scramble keyword = map unShiftChar $ cipherPhrase scramble keyword
  where unShiftChar :: (Char, Char) -> Char
        unShiftChar (' ', ' ') = ' '
        unShiftChar (x, y)     = findLetter ((findNum x) - (findNum y))
