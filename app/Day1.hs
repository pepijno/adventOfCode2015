module Main where

import Lib hiding (parse)

parse :: Char -> Int
parse '(' = 1
parse ')' = -1
parse _ = 0

solve1 :: [String] -> Int
solve1 = sum . map parse . head

solve2 :: [String] -> Int
solve2 = length . takeWhile (/=(-1)) . scanl (+) 0 . map parse . head

main :: IO()
main = mainWrapper "day1" solve1 solve2
