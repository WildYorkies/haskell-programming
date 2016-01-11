-- listIntermission2.hs
module ListIntermission2 where

--Using takeWhile and dropWhile, write a function that takes a string and returns a list of strings, using spaces to separate the elements of the string into words, as in the following sample:
-- *Main> myWords "all i wanna do is have some fun"
-- ["all","i","wanna","do","is","have","some","fun"]

listOutWords :: [Char] -> [[Char]]
listOutWords s
  | length s == 0 = [] 
  | otherwise     = (takeWord s) : listOutWords (removeWord s)
                      where takeWord   = takeWhile (/= ' ')
                            removeWord = (dropWhile (== ' ')) . (dropWhile (/= ' '))
                            
                            
                            
-- write a function that takes a string and returns a list of strings, using newline separators to break up the string as in the following

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?

-- Implement this
myLines :: [Char] -> [[Char]]
myLines sen
  | length sen == 0 = []
  | otherwise       = (takeLine sen) : myLines (removeLine sen)
                        where takeLine   = takeWhile (/= '\n')
                              removeLine = (dropWhile (== '\n')) . (dropWhile (/= '\n'))

-- This is what we want 'myLines sentences' to equal
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

-- The main function here is a small test
-- to ensure you've written your function
-- correctly.
main :: IO ()
main =
  print $ "Are they equal? "
            ++ show (myLines sentences == shouldEqual)