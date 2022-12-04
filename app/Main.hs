module Main where

import qualified DayOne (answer)
import qualified Day2

main :: IO ()
main = do
  DayOne.answer
  Day2.easyAnswer
  Day2.hardAnswer
