module Main where

import Lib
import Data.Char
import Data.List

step :: [Int] -> [Int]
step xs = concat $ map (\x -> [length x, head x]) $ group xs

solve :: Int -> [Int] -> Int
solve n = length . last . take (n + 1) . iterate step

solve1 :: [String] -> Int
solve1 = solve 40 . map digitToInt . head

solve2 :: [String] -> Int
solve2 = solve 50 . map digitToInt . head

main :: IO()
main = mainWrapper "day10" solve1 solve2
