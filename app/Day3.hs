module Main where

import Lib
import Data.List

moveSanta :: (Int, Int) -> Char -> (Int, Int)
moveSanta (x, y) '^' = (x, y - 1)
moveSanta (x, y) '>' = (x + 1, y)
moveSanta (x, y) 'v' = (x, y + 1)
moveSanta (x, y) '<' = (x - 1, y)
moveSanta pos _ = pos

houses :: String -> [(Int, Int)]
houses = scanl moveSanta (0,0)

solve1 :: [String] -> Int
solve1 = length . nub . houses . head

extractMoves :: (Int -> Bool) -> [a] -> [a]
extractMoves f = map snd . filter (f . fst) . zip [1..]

solve2 :: [String] -> Int
solve2 xs = length . nub $ (houses sms) ++ (houses rsms)
  where
    sms  = extractMoves odd $ head xs
    rsms = extractMoves even $ head xs

main :: IO()
main = mainWrapper "day3" [solve1, solve2]
