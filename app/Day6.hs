{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import qualified Data.Map as M
import Control.Applicative ((<|>))

type Action = Bool -> Bool
type Coord = (Int, Int)
data Instruction = Instruction { action :: Action, start :: Coord, end :: Coord }

parseAction :: Parser Action
parseAction = match (string "turn on") (const True)
  <|> match (string "turn off") (const False)
  <|> match (string "toggle") not
    where match s a = pure a <* s

parseLight :: Parser Coord
parseLight = (,) <$> (integer <* char ',') <*> integer

parseInstruction :: Parser Instruction
parseInstruction = Instruction <$> parseAction <*> (string " " *> parseLight <* string " through ") <*> parseLight

inSquare :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
inSquare (p1, p2) (x1, y1) (x2, y2) = inRange p1 x1 x2 && inRange p2 y1 y2

executeInstruction :: M.Map (Int, Int) Bool -> Instruction -> M.Map (Int, Int) Bool
executeInstruction m i = foldl (\n k -> M.adjust (action i) k n) m $ [(x, y) | x <- [x1..x2], y <- [y1..y2]]
  where (x1, y1) = start i
        (x2, y2) = end i

initMap :: M.Map (Int, Int) Bool
initMap = M.fromList [((x, y), False) | x <- [0..999], y <- [0..999]]

solve1 :: [String] -> Int
solve1 = length . filter id . map snd . M.toList . foldl executeInstruction initMap . map (unsafeParse parseInstruction)

type ActionElf = Int -> Int
data InstructionElf = InstructionElf { actionElf :: ActionElf, startElf :: Coord, endElf :: Coord }

parseActionElf :: Parser ActionElf
parseActionElf = match (string "turn on") ((+) 1)
  <|> match (string "turn off") (\x -> max 0 (x - 1))
  <|> match (string "toggle") ((+) 2)
    where match s a = pure a <* s

parseInstructionElf :: Parser InstructionElf
parseInstructionElf = InstructionElf <$> parseActionElf <*> (string " " *> parseLight <* string " through ") <*> parseLight

executeInstructionElf :: M.Map (Int, Int) Int -> InstructionElf -> M.Map (Int, Int) Int
executeInstructionElf m i = foldl (\n k -> M.adjust (actionElf i) k n) m $ [(x, y) | x <- [x1..x2], y <- [y1..y2]]
  where (x1, y1) = startElf i
        (x2, y2) = endElf i

initMapElf :: M.Map (Int, Int) Int
initMapElf = M.fromList [((x, y), 0) | x <- [0..999], y <- [0..999]]

solve2 :: [String] -> Int
solve2 = sum . map snd . M.toList . foldl executeInstructionElf initMapElf . map (unsafeParse parseInstructionElf)

main :: IO()
main = mainWrapper "day6" [solve2]
