module Day02 where

--import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)
import Util (inputRaw)

import Data.Array.IArray
import Data.List.Split
import Debug.Trace

type Value = Int

-- | Read the input file
input :: (Array Int Value)
input = arrayFromList $ map read $ splitOn "," $ head $ inputRaw "input/Day02.txt"
  where
    arrayFromList l = listArray (0,(length l) - 1) l


-- | Part 1
solve1 :: (Array Int Value) -> Int
solve1 l = executeProgram 0 $ subInitialElements l
  where
    subInitialElements l' = l' // [(1,12),(2,2)]
    executeProgram pos l' =
      (trace $ "\n" ++ show pos ++ show l') $ if code == 99
                   then l' ! 0
                   else
                     if code == 1
                     then executeProgram (pos + 4) (updatePos pos l' (+))
                     else
                       if code == 2
                       then executeProgram (pos + 4) (updatePos pos l' (*))
                       else error "invalid"
      where code = l' ! pos


updatePos :: Int -> (Array Int Value) -> (Int -> Int -> Int) -> (Array Int Value)
updatePos pos l f = l // [(l ! p3,res)]
  where
    res = f (l ! (l ! p1)) (l ! (l ! p2))
    p1 = pos + 1
    p2 = pos + 2
    p3 = pos + 3

run1 :: IO ()
run1 = do
  --(time, result) <- timeItT $ evaluate (solve input)
  result <- evaluate $ solve1 input
  printf "Day01-Part1: (%d)\n" result
