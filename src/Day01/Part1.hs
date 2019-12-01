module Day01.Part1 where

--import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)
import Util (inputRaw)

type Mass = Double

-- | Read the input file (and handle the +)
input :: [Mass]
input = (map processLine . inputRaw) "input/Day01.txt" where
  processLine line = read line
  -- processLine line@(sign:number)
    -- | sign == '+' = read number
    -- | otherwise = read line
  --processLine [] = error "No input"


solve :: [Mass] -> Integer
solve fs = foldl (+) 0 (map getFuel fs)
  where getFuel mass = (floor (mass / 3)) - 2

run :: IO ()
run = do
  --(time, result) <- timeItT $ evaluate (solve input)
  result <- evaluate $ solve input
  printf "Day01-Part1: (%d)\n" result
