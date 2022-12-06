module Day4 where

import Data.List (intersect)

parseContents :: String -> [((Int, Int), (Int, Int))]
parseContents = map (parseLine . break (== ',')) . lines
  where
    toInts (x, y) = (read x, (read . tail) y)
    parseLine (x, y) = (toInts . break (== '-') $ x, (toInts . break (== '-') . tail) y)

contains :: Ord a => (a, a) -> (a, a) -> Bool
contains (a, b) (c, d) = (a <= c && b >= d) || (a >= c && b <= d)

overlap :: (Enum a, Eq a) => (a, a) -> (a, a) -> Bool
overlap (a, b) (c, d) = any (`elem` [a .. b]) [c .. d]

easyAnswer :: IO ()
easyAnswer = do
  contents <- readFile "data/day4.txt"
  let parsed = parseContents contents
  let answer = length $ filter (uncurry contains) parsed
  print "The easy answer for day 4 is:"
  print answer

hardAnswer :: IO ()
hardAnswer = do
  contents <- readFile "data/day4.txt"
  let parsed = parseContents contents
  let answer = length $ filter (uncurry overlap) parsed
  print "The hard answer for day 4 is:"
  print answer