module Day5 where

easyAnswer :: IO ()
easyAnswer = do
  contents <- readFile "data/day5.txt"
  let parsed = parseContents contents
  print "The easy answer for day 5 is:"
  print answer

hardAnswer :: IO ()
hardAnswer = do
  contents <- readFile "data/day5.txt"
  let parsed = parseContents contents
  print "The hard answer for day 5 is:"
  print answer