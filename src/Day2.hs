module Day2 where

-- All solutions

data Play = Scissors | Rock | Paper
  deriving (Show, Eq, Ord, Bounded)

data GameResult = Loss | Draw | Win
  deriving (Show, Eq)

data Round = Rd Play Play
  deriving (Show, Eq)

data Round2 = Rd2 Play GameResult
  deriving (Show, Eq)

compare' :: (Bounded a, Ord a) => a -> a -> Ordering
compare' x y
  | x == minBound && y == maxBound = GT
  | x == maxBound && y == minBound = LT
  | otherwise = compare x y

evalRound :: Round -> GameResult
evalRound (Rd p1 p2) = case compare' p1 p2 of
  EQ -> Draw
  GT -> Loss
  LT -> Win

reverseResult :: Round2 -> Round
reverseResult (Rd2 p1 c)
  | c == Win = Rd p1 (head $ filter (\p -> compare' p1 p == LT) plays)
  | c == Loss = Rd p1 (head $ filter (\p -> compare' p1 p == GT) plays)
  | otherwise = Rd p1 p1
  where
    plays = [Rock, Paper, Scissors]

gameScore :: GameResult -> Int
gameScore Loss = 0
gameScore Draw = 3
gameScore Win = 6

shapeScore :: Round -> Int
shapeScore (Rd _ Rock) = 1
shapeScore (Rd _ Paper) = 2
shapeScore (Rd _ Scissors) = 3

-- Parsing (First solution)

parsePlay :: Char -> Play -- not safe lol
parsePlay s
  | s == 'A' || s == 'X' = Rock
  | s == 'B' || s == 'Y' = Paper
  | otherwise = Scissors

parseInput :: String -> [Round]
parseInput = map (\s -> Rd (parsePlay (s !! 0)) (parsePlay (s !! 2))) . lines

-- Parsing (Second solution)

parseOutcome :: Char -> GameResult
parseOutcome s = case s of
  'X' -> Loss
  'Y' -> Draw
  'Z' -> Win

parseInput2 :: String -> [Round2]
parseInput2 = map (\s -> Rd2 (parsePlay (s !! 0)) (parseOutcome (s !! 2))) . lines

-- Solutions

easyAnswer :: IO ()
easyAnswer = do
  contents <- readFile "data/day2.txt"
  let parsed = parseInput contents
  let score = sum . map (\x -> shapeScore x + (gameScore . evalRound) x) $ parsed
  putStrLn "The easy answer for day 2 is:"
  print score

hardAnswer :: IO ()
hardAnswer = do
  contents <- readFile "data/day2.txt"
  let parsed = map reverseResult $ parseInput2 contents
  let score = sum . map (\x -> shapeScore x + (gameScore . evalRound) x) $ parsed
  putStrLn "The hard answer for day 2 is:"
  print score
