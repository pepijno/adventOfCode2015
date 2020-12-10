module Main where

import Lib
import Data.List

isNice1 :: String -> Bool
isNice1 s = containsVowels && containsDoubles && notContains
  where containsVowels = (length . filter (`elem` "aeiou") $ s) >= 3
        containsDoubles = (length . filter ((<) 1 . length ) $ group s) >= 1
        notContains = not . any (flip isInfixOf s) $ ["ab", "cd", "pq", "xy"]

solve1 :: [String] -> Int
solve1 = length . filter isNice1

containsDouble :: String -> Bool
containsDouble (x:y:rest) = (isInfixOf [x,y] rest) || containsDouble (y:rest)
containsDouble _ = False

containsRepeat :: String -> Bool
containsRepeat (x:y:z:rest) = (x == z) || containsRepeat (y:z:rest)
containsRepeat _ = False

isNice2 :: String -> Bool
isNice2 s = containsDouble s && containsRepeat s

solve2 :: [String] -> Int
solve2 = length . filter isNice2

main :: IO()
main = mainWrapper "day5" solve1 solve2
