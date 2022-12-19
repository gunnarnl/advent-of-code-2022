module Day8 where

import Data.List (transpose)

checkTree :: Ord a => [[a]] -> (Int, Int) -> Bool
checkTree trees (c, r) = scanTreeLine (seert !! c) r && scanTreeLine (trees !! r) c
  where
    seert = transpose trees

scanTreeLine :: Ord a => [a] -> Int -> Bool
scanTreeLine line ix = scanSections . splitAt ix $ line
  where
    height = line !! ix
    scanSections (f, _ : s) = any (>= height) f && any (>= height) s

scanTreeLineScenic :: Ord a => [a] -> Int -> Int
scanTreeLineScenic line ix = scanSections . splitAt ix $ line
  where
    height = line !! ix
    takeWhilePlus p = foldr (\x ys -> if p x then x : ys else [x]) []
    scanSections (f, _ : s) = (length . takeWhilePlus (< height) $ reverse f) * (length . takeWhilePlus (< height) $ s)

checkTreeScenic :: Ord a => [[a]] -> (Int, Int) -> Int
checkTreeScenic trees (c, r) = scanTreeLineScenic (seert !! c) r * scanTreeLineScenic (trees !! r) c
  where
    seert = transpose trees

easyAnswer :: IO ()
easyAnswer = do
  contents <- readFile "data/day8.txt"
  let trees = lines contents
  let ixs = (,) <$> [0 .. (length . head $ trees) - 1] <*> [0 .. length trees - 1]
  let answer = sum . map (fromEnum . not . checkTree trees) $ ixs
  print "The easy answer for day 8 is:"
  print answer

hardAnswer :: IO ()
hardAnswer = do
  contents <- readFile "data/day8.txt"
  let trees = lines contents
  let ixs = (,) <$> [0 .. (length . head $ trees) - 1] <*> [0 .. length trees - 1]
  let answer = maximum . map (checkTreeScenic trees) $ ixs
  print "The hard answer for day 8 is:"
  print answer