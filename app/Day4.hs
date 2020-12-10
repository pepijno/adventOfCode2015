module Main where

import Lib
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Digest.Pure.MD5

hasCorrectHash :: String -> Int -> Int -> Bool
hasCorrectHash code n x = (==) zeroes . take n . show . md5 . BLU.fromString $ code ++ (show x)
  where zeroes = take n (repeat '0')

solve1 :: [String] -> Int
solve1 xs = head . filter (hasCorrectHash code 5) $ [1..]
  where code = head xs

solve2 :: [String] -> Int
solve2 xs = head . filter (hasCorrectHash code 6) $ [1..]
  where code = head xs

main :: IO()
main = mainWrapper "day4" solve1 solve2
