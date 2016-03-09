-- jammin.hs

module Jammin where
import Data.List

data Fruit =
  Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Ord, Show)

data JamJars = 
  Jam { fruit :: Fruit
      , jars :: Int }
      deriving (Eq, Ord, Show)

row1 = Jam Peach 3
row2 = Jam Apple 3
row3 = Jam Blackberry 12
row4 = Jam Plum 5
row5 = Jam Apple 25
row6 = Jam Peach 6
allJam = [row1, row2, row3, row4, row5, row6]

totalJamJars :: [JamJars] -> Int
totalJamJars l = sum $ map jars l

mostRow :: [JamJars] -> JamJars
mostRow l = foldr (\(Jam f j) (Jam f' j') -> case compare j j' of 
                                               GT -> (Jam f j)
                                               LT -> (Jam f' j')
                                               EQ -> (Jam f j))
                  (Jam Peach 0) l

compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k _) (Jam k' _) = compare k k'

sortedJams = sortBy compareKind allJam
groupedJams = groupBy (==) sortedJams
