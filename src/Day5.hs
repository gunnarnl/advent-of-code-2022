module Day5 where
import Data.List ( transpose )

-- what a pain in the ass
parseContents :: String -> ([String],[[Int]])
parseContents = parseSides . break null . lines
  where
    parseBox = filter (not . null) . map (takeWhile (`elem` ['A'..'Z'])) . transpose . tail . reverse
    parseRule = map (reverse . map (read . last) . takeWhile (not . null) . iterate (init . init) . words) . tail
    parseSides (boxes, rules) = (parseBox boxes, parseRule rules)


applyRule :: [Int] -> [String] -> [String]
applyRule (n:s1:s2:_) s = newStacks s n s1 s2
  where listToMove =  take n . reverse $ (s !! (s1-1))
        oldList =  reverse . drop n . reverse $ (s !! (s1-1))
        newList = (s !! (s2-1)) ++ listToMove
        changeList n' r l = take (n'-1) l ++ r : drop n' l
        newStacks s' n' s1' s2'
          | s1' < s2' = changeList s2' newList . changeList s1' oldList $ s'
          | otherwise = changeList s2' newList . changeList s1' oldList $ s'


applyRule9001 :: [Int] -> [String] -> [String]
applyRule9001 (n:s1:s2:_) s = newStacks s n s1 s2
  where listToMove =  reverse . take n . reverse $ (s !! (s1-1))
        oldList =  reverse . drop n . reverse $ (s !! (s1-1))
        newList = (s !! (s2-1)) ++ listToMove
        changeList n' r l = take (n'-1) l ++ r : drop n' l
        newStacks s' n' s1' s2'
          | s1' < s2' = changeList s2' newList . changeList s1' oldList $ s'
          | otherwise = changeList s2' newList . changeList s1' oldList $ s'


easyAnswer :: IO ()
easyAnswer = do
  contents <- readFile "data/day5.txt"
  let parsed = parseContents contents
  let folded = foldr applyRule (fst parsed) (reverse . snd $ parsed)
  let tops = map last folded
  print "The easy answer for day 5 is:"
  print tops

hardAnswer :: IO ()
hardAnswer = do
  contents <- readFile "data/day5.txt"
  let parsed = parseContents contents
  let folded = foldr applyRule9001 (fst parsed) (reverse . snd $ parsed)
  let tops = map last folded
  print "The hard answer for day 5 is:"
  print tops