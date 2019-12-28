module Day04 where

--import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)
import Util (inputRaw)
import Data.Char (digitToInt)
import Debug.Trace

iMin = "146810"
iMax = "612564"
inputSet = [(read iMin :: Int) .. (read iMax :: Int)]

nonDecreasing :: Int -> Bool
nonDecreasing s = ha (show s)
  where ha [] = True
        ha (x:y:xs) =
          if (digitToInt x) > (digitToInt y)
          then False
          else ha (y:xs)
        ha (x:xs) = True

hasAdjacent :: Int -> Bool
hasAdjacent s = ha (show s)
  where ha [] = False
        ha (x:y:xs) =
          if x == y
          then True
          else ha (y:xs)
        ha (x:xs) = False

notThreeAdjacent :: Int -> Bool
notThreeAdjacent s = ha (show s)
  where ha (a:b:c:d:e:f:xs) =
          if (a == b && b /= c) ||
             (a /= b && b == c && c /= d) ||
             (b /= c && c == d && d /= e) ||
             (c /= d && d == e && e /= f) ||
             (d /= e && e == f)
          then True
          else False
        --ha (x:y:z:xs) = False
        --ha (x:y:xs) = False
        ha _ = False

t (x:y:xs) = trace (show (x) ++ show (y) ++ show (xs)) t (y:xs)
t [] = False
 

run1 :: IO ()
run1 = do
  print $ length $ filter nonDecreasing $ filter hasAdjacent inputSet

run2 :: IO ()
run2 = do
  print $ length $ filter nonDecreasing $ filter notThreeAdjacent inputSet
