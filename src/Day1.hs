module Day1 (answer) where

getFileContents :: String -> IO [String]
getFileContents s = do
    contents <- readFile s
    return $ lines contents

splitWhenAndModify :: (a -> b) -> (a -> Bool) -> [a] -> [[b]]
splitWhenAndModify mod cond x = helper x []
    where helper [] a = [a]
          helper (x:xs) a
            | cond x = a : helper xs []
            | otherwise = helper xs (mod x:a)

parseData :: [String] -> [[Integer]]
parseData = splitWhenAndModify read cond
    where cond x = x == ""

reverseSort' :: (Ord a) => [a] -> [a]
reverseSort' [] = []
reverseSort' (x:xs) = reverseSort' high ++ [x] ++ reverseSort' low
    where high = filter (> x) xs
          low = filter (<= x) xs

answer :: IO ()
answer = do
    contents <- getFileContents "data/day1.txt"
    let parsed = parseData contents
    let sums = map sum parsed
    print (sum . take 3 $ reverseSort' sums)
    
