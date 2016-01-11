-- zippingExercises.hs
module ZippingExercises where

myZip :: [a] -> [b] -> [(a,b)]
myZip  _      []      = []
myZip []      _       = []
myZip (x:xs) (x':xs') = (x,x') : myZip xs xs'

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f  _      []      = []
myZipWith f []      _       = []
myZipWith f (x:xs) (x':xs') = f x x' : myZipWith f xs xs'

