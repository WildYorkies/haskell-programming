-- db_exercises.hs
module Db_exercises where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 45
  , DbNumber 121
  , DbNumber 12
  ]

--filterDbDate :: [DatabaseItem] -> [UTCTime]
--filterDbDate dbList = map grabTime (filter isDate dbList)
--   where isDate (DbDate d) = True
--         isDate _          = False
--         grabTime (DbDate (UTCTime greg secs)) = UTCTime greg secs

filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' dbList = foldr (\x theRest -> case x of DbDate y -> y : theRest; _ -> theRest) [] dbList

--mostRecent :: [DatabaseItem] -> UTCTime
--mostRecent dbList = maximum (map grabTime (filter isDate dbList))
--   where isDate (DbDate d) = True
--         isDate _          = False
--         grabTime (DbDate (UTCTime greg secs)) = UTCTime greg secs

mostRecent' :: [DatabaseItem] -> UTCTime
mostRecent' dbList = maximum $ foldr (\x theRest -> case x of DbDate y -> y : theRest; _ -> theRest) [] dbList
                     

--filterDbNumber :: [DatabaseItem] -> [Integer]
--filterDbNumber dbList = map grabNumber (filter isNumber dbList)
--   where isNumber (DbNumber i) = True
--         isNumber _            = False
--         grabNumber (DbNumber i) = i

filterDbNumber' :: [DatabaseItem] -> [Integer]
filterDbNumber' dbList = foldr (\x ts -> case x of DbNumber t -> t : ts
                                                   _          -> ts) 
                               [] dbList

sumDb :: [DatabaseItem] -> Integer
sumDb dbList = sum (map grabNumber (filter isNumber dbList))
   where isNumber (DbNumber i) = True
         isNumber _            = False
         grabNumber (DbNumber i) = i

sumDb' :: [DatabaseItem] -> Integer
sumDb' dbList = foldr (\x theRest -> case x of DbNumber y -> y + theRest
                                               _          -> theRest)
                      0 dbList

avgDb :: [DatabaseItem] -> Double
avgDb dbList = (fromIntegral (sum newList)) / (fromIntegral (length newList))
   where newList = map grabNumber (filter isNumber dbList)
            where isNumber (DbNumber i) = True
                  isNumber _            = False
                  grabNumber (DbNumber i) = i

avgDb' :: [DatabaseItem] -> Integer
avgDb' dbList = foldr (\x theRest -> case x of DbNumber y -> y + theRest
                                               _          -> theRest)
                      0 dbList



