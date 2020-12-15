module Main where

import Lib
import Data.Char
import Data.List
import Data.List.Split

parseLine :: String -> (String, String)
parseLine xs = (w !! 0, w !! 2)
  where w = words xs

allReps :: String -> String -> (String, String) -> [String]
allReps _ [] _ = []
allReps r s@(x:xs) a@(from, to)
  | take l s == from = (r ++ to ++ drop l s):(allReps (r ++ [x]) xs a)
  | otherwise = allReps (r ++ [x]) xs a
  where l = length from

solve1 :: [String] -> Int
solve1 xs = length . nub . concat . map (allReps "" m ) $ reps
  where ps = groupPairs xs
        m = head $ last ps
        reps = map parseLine $ head ps

occs :: Eq a => [a] -> [[a]] -> [Int]
occs str = map (count str)
  where count s x = length (splitOn x s) - 1

solve2 :: [String] -> Int
solve2 xs = total - ((o !! 0) + (o !! 1) + 2 * (o !! 2)) - 1
  where ps = groupPairs xs
        m = head $ last ps
        total = length $ filter isUpper m
        o = occs m ["Rn", "Ar", "Y"]

main :: IO()
main = mainWrapper "day19" solve1 solve2
