module Day10 where
import Data.List ( foldl' )

data Pixel = Lit | Dark 

instance Show Pixel where
    show Lit = "#"
    show Dark = "."

logInst :: [(Int, Int)] -> String -> [(Int, Int)]
logInst ((c,v):rest) inst
    | inst == "noop" = (c+1,v):((c,v):rest)
    | otherwise = (c+2,v+val):(c+1,v):((c,v):rest)
    where val = read . dropWhile (/=' ') $ inst

logInsts :: [String] -> [(Int, Int)]
logInsts = foldl' logInst [(0, 1)]

sumLogs :: [(Int, Int)] ->  Int
sumLogs log = sum filtered
    where 
        steps = map (subtract 1) [20,60,100,140,180,220]
        filtered = [(x+1)*y | (x,y) <- log, x `elem` steps]

drawPixel :: (Int, Int) -> Pixel
drawPixel (c, v)
    | (c `mod` 40) `elem` [v-1,v,v+1] = Lit
    | otherwise = Dark

easyAnswer :: IO ()
easyAnswer = do
  contents <- readFile "data/day10.txt"
  let answer = logInsts . lines $ contents
  print "The easy answer for day 10 is:"
  print $ sumLogs answer

hardAnswer :: IO ()
hardAnswer = do
  contents <- readFile "data/day10.txt"
  let logs = logInsts . lines $ contents
  let pixels = concatMap (show . drawPixel) . reverse $ logs
  print "The hard answer for day 10 is:"
  print $ take 40 pixels
  print $ take 40 . drop 40 $ pixels
  print $ take 40 . drop 80 $ pixels
  print $ take 40 . drop 120 $ pixels
  print $ take 40 . drop 160 $ pixels
  print $ take 40 . drop 200 $ pixels
