module Main where

import Lib
import Data.List
import Data.List.Split

toArea :: [Int] -> Int
toArea (a:b:c:_) = 2 * a * b + 2 * b * c + 2 * a * c + minSide
  where
    minSide = minimum [a * b, b * c, a * c]

solve1 :: [String] -> Int
solve1 = sum . map (toArea . map read . splitOn "x")

toRibbon :: [Int] -> Int
toRibbon sides = (+) (product sides) . (*) 2 . sum . take 2 . sort $ sides

solve2 :: [String] -> Int
solve2 = sum . map (toRibbon . map read . splitOn "x")

main :: IO()
main = mainWrapper "day2" [solve1, solve2]
