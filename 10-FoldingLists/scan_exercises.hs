-- scan_exercises.hs
module Scan_exercises where

fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

fibs20 = take 20 fibs
fibsLess100 = filter (<100) fibs

--facScan = x : scanl (*) 1 (facScan (x+1))
--safeFacScan = take 7 (facScan 1)