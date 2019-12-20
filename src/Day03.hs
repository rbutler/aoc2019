module Day03 where

--import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)
import Util (inputRaw)

import Data.Array.IArray
import Data.List.Split
import Debug.Trace

import qualified Data.Map.Strict as Map

data Motion = Motion Direction Int
  deriving (Eq, Read, Show)

data Direction = U | D | L | R
  deriving (Eq, Read, Show)

type Coord = (Int, Int)

type CoordMap = Data.Map Coord Bool

dirToAction :: Direction -> Coord
dirToAction U = (0, -1)
dirToAction D = (0, 1)
dirToAction L = (-1, 0)
dirToAction R = (1, 0)

move :: Coord -> Coord -> Coord
move (x,y) (dx, dy) = (x+dx, y+dy)

manhattan :: Coord -> Int
manhattan (x,y) = abs x + abs y

type Parameter = Int
-- | Read the input file
input :: [[Motion]]
input = (map (map processLine . splitLine) . inputRaw) "input/Day03.txt"
  where
    processLine line@(direction:number)
      | direction == 'U' = Motion U $ read number
      | direction == 'D' = Motion D $ read number
      | direction == 'L' = Motion L $ read number
      | direction == 'R' = Motion R $ read number
      | otherwise = read line
    processLine [] = error "No input"
    splitLine = splitOn ","

addMotion :: CoordMap -> Coord -> Motion -> CoordMap
addMotion coords current mot =

run1 :: IO ()
run1 = do
  let map1 = foldr addMotion (Map.empty :: CoordMap) (first input)
  --result <- evaluate $ input
  print result
