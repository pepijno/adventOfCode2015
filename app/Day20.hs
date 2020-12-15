module Main where

import Lib
import Data.List
import Control.Arrow

divisors :: Int -> [Int]
divisors n = map (*10) $ nub $ concat [ [x, n `div` x] | x <- [1..limit], n `rem` x == 0 ]
     where limit = (floor . sqrt . fromIntegral) n

solve1 :: [String] -> Int
solve1 xs = ((+1) . length . takeWhile ((>) limit) . map (sum . divisors)) [1..]
  where limit = (read . head) xs

divisors' :: Int -> [Int]
divisors' n = map (*11) $ filter (\x -> n `div` x <= 50) $ nub $ concat [ [x, n `div` x] | x <- [1..limit], n `rem` x == 0 ]
     where limit = (floor . sqrt . fromIntegral) n

solve2 :: [String] -> Int
solve2 xs = ((+1) . length . takeWhile ((>) limit) . map (sum . divisors')) [1..]
  where limit = (read . head) xs

main :: IO()
main = mainWrapper "day20" solve1 solve2
