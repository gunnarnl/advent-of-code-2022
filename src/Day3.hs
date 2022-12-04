module Day3 where
import Data.Char ( ord )
import Data.List ( intersect, nub )

splitRucksack :: String -> (String, String)
splitRucksack s = splitAt (length s `div` 2) s

charsInBoth :: (String, String) -> [Char]
charsInBoth (l, r) = nub $ intersect l r

getPriority :: Char -> Int
getPriority c =
    if c >= 'a' then ord c - 96
    else ord c - 38

easyAnswer :: IO ()
easyAnswer = do
    contents <- readFile "data/day3.txt"
    let chars = concatMap (charsInBoth . splitRucksack) . lines $ contents
    print "The easy answer for day 3 is:"
    print $ sum $ map getPriority chars

-- Second problem solution, generalized
-- Doesn't work in place of the above though because of
-- tuple's weird foldability properties...
intersectFoldable :: (Foldable t, Eq a) => t [a] -> [a]
intersectFoldable = nub . foldr intersect' []
    where intersect' x [] = x
          intersect' x y = x `intersect` y

chunkBy :: Int -> [a] -> [[a]]
chunkBy _ [] = []
chunkBy n xs = x : chunkBy n rest
    where (x, rest) = splitAt n xs

hardAnswer :: IO ()
hardAnswer = do
    contents <- readFile "data/day3.txt"
    let chars = concatMap intersectFoldable . chunkBy 3 $ lines  contents
    print "The hard answer for day 3 is:"
    print $ sum $ map getPriority chars
