module Day7 where
import Data.List ( isPrefixOf )

data FS = File String Int | Dir String [FS] 
    deriving (Show)

parseData :: [String] -> ([String], FS) -> ([String], FS)
parseData [] x = x
parseData (cmd:cmds) (pwd:hist, fs)
 | "$ cd .." == cmd = parseData cmds (hist, fs)
 | "$ cd /" == cmd = parseData cmds (["/"], fs)
 | "$ cd" `isPrefixOf` cmd = parseData cmds (dirName:(pwd:hist), addedDir)
 | "$ ls" `isPrefixOf` cmd = parseData cmds (pwd:hist, fs)
 | "dir" `isPrefixOf` cmd = parseData cmds (pwd:hist, fs)
 | otherwise = parseData cmds (pwd:hist, newFS)
 where 
    dirName = drop 5 cmd
    addedDir = addItemToFS pwd (Dir dirName []) fs
    parseFile (size, _:filename) = File filename (read size)
    parsedItem = parseFile $ break (==' ') cmd
    newFS = addItemToFS pwd parsedItem fs

test = Dir "d" [File "a" 2, File "b" 3, Dir "e" [File "g" 200]]

test2 = addItemToFS "e" (File "c" 4) test

addItemToFS :: String -> FS -> FS -> FS
addItemToFS loc item (Dir pwd cnt)
    | loc == pwd = Dir pwd (item:cnt)
    | null cnt = Dir pwd cnt
    | otherwise = Dir pwd (map (addItemToFS loc item) cnt)
addItemToFS _ _ file = file

sumDir :: FS -> Int
sumDir (File _ size) = size
sumDir (Dir _ []) = 0
sumDir (Dir _ cnt) = sum . map sumDir $ cnt

getDirSizes :: FS -> [(String, Int)] -> [(String, Int)]
getDirSizes (File _ _) l = l
getDirSizes (Dir pwd []) l = (pwd, 0):l
getDirSizes (Dir pwd cnt) l =  (pwd, sumDir (Dir pwd cnt)):l ++ rest
    where rest = concatMap (\x -> getDirSizes x []) cnt

easyAnswer :: IO ()
easyAnswer = do
  contents <- readFile "data/day7_test.txt"
  let parsed = flip parseData (["/"], Dir "/" []) . drop 1 . lines $ contents
  let dirsizes = flip getDirSizes [] . snd $ parsed
  let total =  sum . filter (<=100000) . map snd $ dirsizes
  print "The easy answer for day 7 is:"
  print parsed

hardAnswer :: IO ()
hardAnswer = do
  contents <- readFile "data/day7.txt"
  print "The hard answer for day 7 is:"

-- module Day7 where
-- import Data.List ( isPrefixOf )
-- data File = Dir String [File] | File String Int deriving (Show)

-- getFilesystem :: [String] -> File -> ([String], File)
-- getFilesystem [] fs = ([], fs)
-- getFilesystem (cmd : cmds) (Dir name files)
--     | cmd == "$ cd .."        = (cmds, Dir name files)
--     | "dir"  `isPrefixOf` cmd = getFilesystem cmds (Dir name files)
--     | "$ ls" `isPrefixOf` cmd = getFilesystem cmds (Dir name files)
--     | "$ cd" `isPrefixOf` cmd = getFilesystem ncmds (Dir name (sfs : files))
--     | otherwise = getFilesystem cmds (Dir name (nf : files))
--     where
--         [size, fname] = words cmd
--         nf = File fname (read size)
--         (ncmds, sfs) = getFilesystem cmds (Dir (drop 5 cmd) [])

-- getSize :: File -> Int
-- getSize (File _ size) = size
-- getSize (Dir _ files) = sum  . map getSize $ files

-- getDirSizes :: File -> [Int]
-- getDirSizes (File _ _)    = []
-- getDirSizes (Dir n files) =
--     getSize (Dir n files) : foldl (\a d -> a ++ getDirSizes d) [] files

-- easyAnswer = do
--     input <- snd . flip getFilesystem (Dir "/" []) . drop 2 . lines
--              <$> readFile "data/day7.txt"
--     let dirSizes = getDirSizes input
--     let unused = 70000000 - getSize input
--     print $ sum . filter (<= 100000) $ dirSizes
--     print $ minimum . filter (>= (30000000 - unused)) $ dirSizes