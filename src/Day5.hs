module Day5 where
import Data.List ( transpose )

parseContents :: String -> ([String],[[String]])
parseContents = parseSides . break null . lines
  where
    parseBox = filter (all (`elem` ['A'..'Z']++[' '])) . transpose . tail . reverse
    parseRule = map (map last . takeWhile (not . null) . iterate (drop 2) . words) . tail
    parseSides (boxes, rules) = (parseBox boxes, parseRule rules)

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