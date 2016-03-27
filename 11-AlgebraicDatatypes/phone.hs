-- phone.hs
module Phone where

-- Create a data structure that captures the phone layout above.
-- The data structure should be able to express enough of 
-- how the layout works that you can use it to dictate 
-- the behavior of the functions in the following exercises.
data DaPhone = DaPhone

-- Convert the following conversations into the key presses required to express them.
convo :: [String] 
convo =
  ["Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol lol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Haha thanks just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps = undefined
-- assuming the default phone definition
-- 'a' -> ('2', 1)
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = undefined

-- How many times do digits need to be pressed for each message?
fingerTaps :: [(Digit, Presses)] -> Presses fingerTaps = undefined

-- What was the most popular letter for each message? 
-- What was its cost? 
-- You’ll want to combine reverseTaps and fingerTaps to figure out what it cost in taps. 
-- reverseTaps is a list because you need to press a different button in order to get capitals. 
mostPopularLetter :: String -> Char 
mostPopularLetter = undefined

-- What was the most popular letter overall? What was the most popular word?
coolestLtr :: [String] -> Char 
coolestLtr = undefined

coolestWord :: [String] -> String 
coolestWord = undefined

