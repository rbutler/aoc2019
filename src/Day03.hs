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

type CoordMap = Map.Map Coord Int

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

addMotion :: CoordMap -> Coord -> Int -> Motion -> (CoordMap, Coord, Int)
addMotion coords currentCoord@(x,y) acc (Motion dir n)
  | n == 0 = (coords, currentCoord, acc)
  | dir == U = addMotion (Map.insert (x,y) acc coords) (move currentCoord (0, 1)) (acc + 1) (Motion dir (n - 1))
  | dir == D = addMotion (Map.insert (x,y) acc coords) (move currentCoord (0, -1)) (acc + 1) (Motion dir (n - 1))
  | dir == L = addMotion (Map.insert (x,y) acc coords) (move currentCoord (-1, 0)) (acc + 1) (Motion dir (n - 1))
  | dir == R = addMotion (Map.insert (x,y) acc coords) (move currentCoord (1, 0)) (acc + 1) (Motion dir (n - 1))
  | otherwise = error "this broke"

runMotions :: CoordMap -> Coord -> Int -> [Motion] -> CoordMap
runMotions coords _ _ [] = coords
runMotions coords currentCoord acc (m:ms) = runMotions cm p a ms
  where (cm, p, a) = addMotion coords currentCoord acc m

run1 :: IO ()
run1 = do
  let map1 = runMotions (Map.empty :: CoordMap) (0,0) 1 (input !! 0)
  let map2 = runMotions (Map.empty :: CoordMap) (0,0) 1 (input !! 1)
  let ix = Map.intersection map1 map2
  let mans = map manhattan $ Map.keys ix
  print $ show mans

run2 :: IO ()
run2 = do
  let map1 = runMotions (Map.empty :: CoordMap) (0,0) 0 (input !! 0)
  let map2 = runMotions (Map.empty :: CoordMap) (0,0) 0 (input !! 1)
  let ix = Map.intersectionWith (+) map1 map2
  print $ show ix
  let mans = Map.elems ix
  --let mans = Map.traverseWithKey (\k v -> )
  --print $ show mans
  print $ minimum mans
