module Main where

import Lib
import Data.Char
import Data.Word
import Data.Bits
import Control.Applicative
import qualified Data.Map as Map

data Term = Value Word16 | Wire String deriving (Show)

data Line = Single (Word16 -> Word16) Term | Binary (Word16 -> Word16 -> Word16) Term Term | Constant Word16

instance Show Line where
  show (Single f t) = "Single " ++ show t
  show (Binary f t1 t2) = "Binary " ++ (show t1) ++ " " ++ show t2
  show (Constant t) = "Constant " ++ show t

fir :: (Word16 -> Int -> Word16) -> (Word16 -> Word16 -> Word16)
fir f a1 a2 = f a1 (fromIntegral a2)

parseTerm :: String -> Term
parseTerm t@(t1 : _)
    | isDigit t1 = Value (read t)
    | isAlpha t1 = Wire t

parseLine :: [String] -> (String, Line)
parseLine [r, "->", line] = (line, Single id (parseTerm r))
parseLine ["NOT", r, "->", line] = (line, Single complement (parseTerm r))
parseLine [r1, "AND", r2, "->", line] = (line, Binary (.&.) (parseTerm r1) (parseTerm r2))
parseLine [r1, "OR", r2, "->", line] = (line, Binary (.|.) (parseTerm r1) (parseTerm r2))
parseLine [r1, "LSHIFT", r2, "->", line] = (line, Binary (fir shiftL) (parseTerm r1) (parseTerm r2))
parseLine [r1, "RSHIFT", r2, "->", line] = (line, Binary (fir shiftR) (parseTerm r1) (parseTerm r2))

eval :: String -> Map.Map String Line -> (Map.Map String Line, Word16)
eval line lines = case Map.lookup line lines of
                    Just (Constant v) -> (lines, v)
                    Just (Single f t) -> let (lines', n) = evalLine t lines
                                          in (lines', f n)
                    Just (Binary f t1 t2) -> let (lines', n1) = evalLine t1 lines
                                              in let (lines'', n2) = evalLine t2 lines'
                                                  in (lines'', f n1 n2)
                    Nothing -> error "not found"

evalLine :: Term -> Map.Map String Line -> (Map.Map String Line, Word16)
evalLine (Value v) lines = (lines, v)
evalLine (Wire w) lines = let (lines', v) = eval w lines
                           in (Map.insert w (Constant v) lines', v)

solve1 :: [String] -> Word16
solve1 = snd . eval "a" . Map.fromList . map (parseLine . words)

solve2 :: [String] -> Word16
solve2 xs = snd . eval "a" . Map.insert "b" (Constant val) . Map.fromList $ parsed
  where parsed = map (parseLine . words) xs
        val = snd . eval "a" . Map.fromList $ parsed

main :: IO()
main = mainWrapper "day7" [solve1, solve2]
