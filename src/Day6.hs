module Day6 where
import Data.List (nub)

scanDifferent :: Int -> String -> (String, Int) -> (String, Int)
scanDifferent bufn [s] (buffer, c) = ("LAST", c+1)
scanDifferent bufn (s:ss) (buffer, c)
    | length buffer < bufn-1 = scanDifferent bufn ss (buffer++[s], c+1)
    | (length . nub $ buffer++[s]) < bufn = scanDifferent bufn ss (tail buffer++[s], c+1)
    | otherwise = (buffer++[s], c+1)

easyAnswer :: IO ()
easyAnswer = do
  contents <- readFile "data/day6.txt"
  let answer = scanDifferent 4 contents ("", 0)
  print "The easy answer for day 6 is:"
  print answer

hardAnswer :: IO ()
hardAnswer = do
  contents <- readFile "data/day6.txt"
  let answer = scanDifferent 14 contents ("", 0)
  print "The hard answer for day 6 is:"
  print answer