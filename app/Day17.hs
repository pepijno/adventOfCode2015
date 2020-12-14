module Main where

import Lib
import Data.List
import Data.Function
import qualified Data.Map as M

solve :: Int -> [Int] -> [[Int]]
solve 0 _ = [[]]
solve _ [] = []
solve n [x] = if x == n then [[n]] else []
solve n (x:xs) = (solve n xs) ++ (map (x:) $ solve (n - x) xs)

solve1 :: [String] -> Int
solve1 = length . solve 150 . map read

-- solve2 :: [String] -> Int
solve2 = length . head . groupBy ((==) `on` length) . sortBy (compare `on` length) . solve 150 . map read

main :: IO()
main = mainWrapper "day17" solve1 solve2
