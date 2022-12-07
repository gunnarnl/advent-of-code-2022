module Day5 where
import Data.List ( transpose )

data Move = Mv Int Int Int

-- what a pain in the ass
parseContents :: String -> ([String],[[Int]])
parseContents = parseSides . break null . lines
  where
    parseBox = filter (not . null) . map (takeWhile (`elem` ['A'..'Z'])) . transpose . tail . reverse
    parseRule = map (reverse . map (read . last) . takeWhile (not . null) . iterate (init . init) . words) . tail
    parseSides (boxes, rules) = (parseBox boxes, parseRule rules)

applyRule :: [String] -> [Int] -> [String]
applyRule s (n:s1:s2:_) = removedList . splitAt s1
  where listToMove = take n . reverse $ (s !! s1)
        oldList = drop n
        newList = (s !! s2) ++ listToMove

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