module Main where

import Lib
import Data.Char
import Data.List

incrementPassword' :: Bool -> [Int] -> [Int]
incrementPassword' False pw = pw
incrementPassword' True [] = [0]
incrementPassword' True (p:pw) = (z `mod` 26):(incrementPassword' (z >= 26) pw)
  where r = p + 1
        z = if r == 8 || r == 14 || r == 11 then r + 1 else r

incrementPassword :: [Int] -> [Int]
incrementPassword = incrementPassword' True

hasThreeGroup :: [Int] -> Bool
hasThreeGroup (a:l@(b:c:rest)) = ((b == (a - 1)) && (c == (b - 1))) || hasThreeGroup l
hasThreeGroup _ = False

validPassword :: [Int] -> Bool
validPassword pw = (g pw > 1) && (hasThreeGroup pw)
  where g = length . filter (>1) . map length . group

strToOrd :: String -> [Int]
strToOrd = map (\x -> ord x - ord 'a')

ordToStr :: [Int] -> String
ordToStr = map (\x -> chr (x + ord 'a'))

nextPassword :: [Int] -> [Int]
nextPassword = head . filter validPassword . iterate incrementPassword

wrapReverse :: ([Int] -> [Int]) -> String -> String
wrapReverse f = reverse . ordToStr . f . strToOrd . reverse

solve1 :: [String] -> String
solve1 = wrapReverse nextPassword . head

solve2 :: [String] -> String
solve2 = wrapReverse (nextPassword . incrementPassword . nextPassword) . head

main :: IO()
main = mainWrapper "day11" solve1 solve2
