{-# LANGUAGE FlexibleContexts #-}
module Day01 where

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


-- | Part 1
solve1 :: [Mass] -> Integer
solve1 fs = foldl (+) 0 (map getFuel fs)
  where getFuel mass = (floor (mass / 3)) - 2

run1 :: IO ()
run1 = do
  --(time, result) <- timeItT $ evaluate (solve input)
  result <- evaluate $ solve1 input
  printf "Day01-Part1: (%d)\n" result

-- | Part 2
solve2 :: [Mass] -> Integer
solve2 fs = foldl (+) 0 (map getFuel fs)
  where getFuel mass = if (calcFuel mass) <= 0
                       then
                         0 :: Integer
                       else
                         (calcFuel mass) + (getFuel $ fromIntegral (calcFuel mass))
        --calcFuel :: Double -> Integer
        calcFuel m = (floor (m / 3)) - 2

run2 :: IO ()
run2 = do
  result <- evaluate $ solve2 input
  printf "Day01-Part2: (%d)\n" result
