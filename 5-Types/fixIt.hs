-- fixIt.hs
module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x > y) then sndString y else fstString x
  where x = "Singin"
        y = "Somewhere"
