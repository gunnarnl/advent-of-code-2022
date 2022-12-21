module Day9 where

import Data.List (nub, sortOn)

type State = (Int, Int)

parseContents :: [String] -> [Char]
parseContents [] = []
parseContents (x : xs) = cs ++ parseContents xs
  where
    (dir : _, num) = splitAt 2 x
    cs = replicate (read num) dir

updateHead :: State -> Char -> State
updateHead (x, y) dir
  | dir == 'R' = (x + 1, y)
  | dir == 'L' = (x - 1, y)
  | dir == 'D' = (x, y - 1)
  | otherwise = (x, y + 1)

updateTail :: State -> State -> State
updateTail (x, y) (hx, hy)
  | tooHigh = (x + signum (hx - x), y + signum (hy - y))
  | otherwise = (x, y)
  where
    tooHigh = abs (hx - x) > 1 || abs (hy - y) > 1

updateAsList :: [State] -> Char -> [State]
updateAsList (s : ss) dir = foldl (\mvd nxt -> mvd ++ [updateTail nxt (last mvd)]) [updateHead s dir] ss

dirFold :: [[State]] -> [Char] -> [[State]]
dirFold = foldl update
  where
    update (cur : rest) dir = updateAsList cur dir : (cur : rest)

easyAnswer :: IO ()
easyAnswer = do
  contents <- readFile "data/day9.txt"
  let folded = dirFold [[(0, 0), (0, 0)]] . parseContents . lines $ contents
  let answer = length . nub . map last $ folded
  print "The easy answer for day 9 is:"
  print answer

hardAnswer :: IO ()
hardAnswer = do
  contents <- readFile "data/day9.txt"
  let folded = dirFold [replicate 10 (0, 0)] . parseContents . lines $ contents
  let answer = length . nub . map last $ folded
  print "The hard answer for day 9 is:"
  print answer

-- First solution

updateTail' :: State -> State -> Char -> State
updateTail' (x, y) (hx, hy) dir
  | tooHigh && dir == 'R' = (hx - 1, hy)
  | tooHigh && dir == 'L' = (hx + 1, hy)
  | tooHigh && dir == 'D' = (hx, hy + 1)
  | tooHigh && dir == 'U' = (hx, hy - 1)
  | otherwise = (x, y)
  where
    tooHigh = abs (hx - x) > 1 || abs (hy - y) > 1

-- Attempt at a second solution
-- Worked on the tests, but not the main problem.
updateTail'' :: State -> State -> State
updateTail'' (x, y) (hx, hy)
  | tooHigh = head . sortOn (eucDistance (x, y)) $ alts
  | otherwise = (x, y)
  where
    eucDistance (x, y) (x', y') = sqrt (fromIntegral (x - x') ** 2 + fromIntegral (y - y') ** 2)
    alts = [(hx + 1, hy), (hx - 1, hy), (hx, hy + 1), (hx, hy - 1)]
    tooHigh = abs (hx - x) > 1 || abs (hy - y) > 1