module Main where

import Lib
import Data.List

moveSanta :: (Int, Int) -> Char -> (Int, Int)
moveSanta (x, y) '^' = (x, y - 1)
moveSanta (x, y) '>' = (x + 1, y)
moveSanta (x, y) 'v' = (x, y + 1)
moveSanta (x, y) '<' = (x - 1, y)
moveSanta pos _ = pos

houses = scanl moveSanta (0,0)

solve1 :: [String] -> Int
solve1 = length . nub . houses . head

extractMoves f = map snd . filter (f . fst) . zip [1..]
extractSantaMoves = extractMoves odd
extractRoboSantaMoves = extractMoves even

solve2 xs = length . nub $ (houses sms) ++ (houses rsms)
  where
    sms  = extractSantaMoves $ head xs
    rsms = extractRoboSantaMoves $ head xs

main :: IO()
main = mainWrapper "day3" [solve1, solve2]
