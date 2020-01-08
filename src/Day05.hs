module Day05 where

--import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)
import Util (inputRaw)

import Data.List.Split
import Debug.Trace

import Data.Array.IArray
import Intcode

input :: Memory
input = arrayFromList $ map read $ splitOn "," $ head $ inputRaw "input/Day05.txt"
  where
    arrayFromList l = listArray (0,(length l) - 1) l


run1 :: IO ()
run1 = do
  --(time, result) <- timeItT $ evaluate (solve input)
  result <- evaluate $ solve1 input
  printf "Day05-Part1: (%d)\n" result

solve1 :: Memory -> Int
solve1 l = startProgram 1 l
