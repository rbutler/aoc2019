-- |

module Intcode where

import Data.Array.IArray

type Parameter = Int
type Memory = Array Int Parameter
type ArgumentModes = (ParameterMode, ParameterMode, ParameterMode)
type Input = Int
type Position = Int

data ParameterMode = Position | Immediate

toP :: Char -> ParameterMode
toP '0' = Immediate
toP '1' = Position
toP _ = error "invalid parameter mode"

toParameterModes :: String -> ArgumentModes
toParameterModes l = go (reverse l)
  where go (x:y:z:xs) = (toP x, toP y, toP z)
        go (x:y:xs) = (toP x, toP y, Immediate)
        go (x:xs) = (toP x, Immediate, Immediate)
        go _ = error "bad parameter parsing"

lastN :: Int -> String -> String
lastN n = reverse . (take n) . reverse
--lastN n xs = foldl (const . drop 1) <*> drop n

--newInstruction :: String -> ArgumentModes -> Instruction
newInstruction :: String -> InstructionCode
-- Add
newInstruction "01" = Add
-- Multiply
newInstruction "02" = Multiply
-- Input
newInstruction "03" = Input
--Output
newInstruction "04" = Output
newInstruction "99" = Halt
newInstruction _ = error "Invalid instruction"

parseOpcode :: Int -> (InstructionCode, ArgumentModes)
parseOpcode intCode = go (show intCode)
  where go code = (newInstruction (lastN 2 code), (toParameterModes (take (length code) code)))
  --where go code = newInstruction (lastN 2 code) (toParameterModes (take (length code) code))



--data Instruction = Add Int Int Int | Multiply | Input | Output | Halt
data InstructionCode = Add | Multiply | Input | Output | Halt


executeProgram pos input l =
  let (code, argModes) = parseOpcode
  if code == 99
  --(trace $ "\n" ++ show pos ++ show l') $ if code == 99
  then l' ! 0
  else
    if code == 1
    then executeProgram (pos + 4) input (updatePos pos l' (+))
    else
      if code == 2
      then executeProgram (pos + 4) input (updatePos pos l' (*))
      else error "invalid"
  where code = l' ! pos

-- | this is the old executeProgram
--executeProgram :: Position -> Input -> Memory -> Memory
executeProgram' pos input l' =
  if code == 99
  --(trace $ "\n" ++ show pos ++ show l') $ if code == 99
  then l' ! 0
  else
    if code == 1
    then executeProgram' (pos + 4) input (updatePos pos l' (+))
    else
      if code == 2
      then executeProgram' (pos + 4) input (updatePos pos l' (*))
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

startProgram = executeProgram 0
