-- ex2.hs
module Ex2 where

main :: IO ()
main = do
  addExcl s1
  fourthLetter s2
  dropFirstNine s3
  where s1 = "hello"
        s2 = "hahahahahaha"
        s3 = "123456789Hi Guys"

addExcl :: String -> IO ()
addExcl s = putStrLn (s ++ "!")

fourthLetter :: String -> IO ()
fourthLetter s = putStrLn [s !! 4]

dropFirstNine :: String -> IO ()
dropFirstNine s = putStrLn (drop 9 s)
