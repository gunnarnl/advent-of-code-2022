module Day5 where
import Data.List ( transpose )

-- what a pain in the ass
parseContents :: String -> ([String],[[Int]])
parseContents = parseSides . break null . lines
  where
    parseBox = filter (not . null) . map (takeWhile (`elem` ['A'..'Z'])) . transpose . tail . reverse
    parseRule = map (reverse . map (read . last) . takeWhile (not . null) . iterate (init . init) . words) . tail
    parseSides (boxes, rules) = (parseBox boxes, parseRule rules)

applyRule :: [String] -> [Int] -> [String]
applyRule s (n:s1:s2) = removedList . splitAt s1
  where move = take n . reverse
        removedList (x,_:ys) = move (s !! s1)
        removedList = 

easyAnswer :: IO ()
easyAnswer = do
  contents <- readFile "data/day5.txt"
  let parsed = parseContents contents
  print "The easy answer for day 5 is:"
  print parsed

-- hardAnswer :: IO ()
-- hardAnswer = do
--   contents <- readFile "data/day5.txt"
--   let parsed = parseContents contents
--   print "The hard answer for day 5 is:"
--   print answer