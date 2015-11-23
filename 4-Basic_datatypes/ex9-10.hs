-- ex9-10.hs

myAbs :: Integer -> Integer
myAbs x = if x >= 0 then x else (negate x)

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f abTup cdTup = doubleTup
  where doubleTup = ((snd abTup, snd cdTup),
                     (fst abTup, fst cdTup))
