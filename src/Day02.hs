module Day02 where

--import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)
import Util (inputRaw)

import Data.Array.IArray
import Data.List.Split
import Debug.Trace

import Data.Array.IArray

type Parameter = Int
type Memory = Array Int Parameter

executeProgram pos l' =
  if code == 99
  --(trace $ "\n" ++ show pos ++ show l') $ if code == 99
  then l' ! 0
  else
    if code == 1
    then executeProgram (pos + 4) (updatePos pos l' (+))
    else
      if code == 2
      then executeProgram (pos + 4) (updatePos pos l' (*))
      else error "invalid"
  where code = l' ! pos

updatePos :: Int -> Memory -> (Int -> Int -> Int) -> Memory
updatePos pos l f = l // [(l ! p3,res)]
  where
    res = f (l ! (l ! p1)) (l ! (l ! p2))
    p1 = pos + 1
    p2 = pos + 2
    p3 = pos + 3


subInitialElements :: Memory -> Int -> Int -> Memory
subInitialElements l noun verb = l // [(1,noun),(2,verb)]

-- | Read the input file
input :: Memory
input = arrayFromList $ map read $ splitOn "," $ head $ inputRaw "input/Day02.txt"
  where
    arrayFromList l = listArray (0,(length l) - 1) l


-- | Part 1
solve1 :: Memory -> Int
solve1 l = executeProgram 0 $ subInitialElements l 12 2

run1 :: IO ()
run1 = do
  --(time, result) <- timeItT $ evaluate (solve input)
  result <- evaluate $ solve1 input
  printf "Day02-Part1: (%d)\n" result

solve2  :: Memory -> Int
solve2 l =  (\(res,x,y) -> (trace $ show res ++ "-" ++ show x ++ "-" ++ show y) 100 * x + y) $ head $ filter (\(res,x,y) -> res == 19690720)[(executeProgram 0 (subInitialElements l x y), x, y) | x <- [0..99], y <- [0..99]]


run2 :: IO ()
run2 = do
  --(time, result) <- timeItT $ evaluate (solve input)
  result <- evaluate $ solve2 input
  printf "Day02-Part2: (%d)\n" result
