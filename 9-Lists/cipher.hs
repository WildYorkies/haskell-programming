-- cipher.hs
module Cipher where

import Data.Char

alphaList = zip ['a'..'z'] [1..26]

caesar :: Int -> [Char] -> [Char]
caesar shiftNum l = map move l
  where move letter
          | difference < 97    = chr ((ord '`') + difference)
          | otherwise          = chr ((ord (toLower letter)) + shiftNum)
              where difference = mod ((ord (toLower letter)) + shiftNum) 122

unCaesar :: Int -> [Char] -> [Char]
unCaesar shiftNum l = map move l
  where move letter
          | difference /= 97 && difference /= 0 = chr ((ord '{') - difference)
          | otherwise                           = chr ((ord (toLower letter)) - shiftNum)
              where difference                  = mod 97 ((ord (toLower letter)) - shiftNum)