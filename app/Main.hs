module Main where

import qualified Day1 (answer)
import qualified Day2 (easyAnswer, hardAnswer)

main :: IO ()
main = do
  Day1.answer
  Day2.easyAnswer
  Day2.hardAnswer
