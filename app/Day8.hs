module Main where

import Lib
import Data.Char

inMem :: String -> Int
inMem x = length (read x :: String)

inString :: String -> Int
inString x = length x

envelop :: (b -> [a] -> (b, [a])) -> b -> [a] -> b
envelop _ a [] = a
envelop f a xs = uncurry (envelop f) (f a xs)

countEscaped :: String -> Int
countEscaped s =
    envelop countNext 0 s
    where
      countNext n "" = (n, "")
      countNext n ('\\' : '\\' : xs) = (n + 1, xs)
      countNext n ('\\' : '"' : xs) = (n + 1, xs)
      countNext n ('\\' : 'x' : d1 : d2 : xs)
          | isHexDigit d1 && isHexDigit d2 = (n + 1, xs)
          | otherwise = error "bad hex escape"
      countNext _ ('\\' : _) = error "bad backslash escape"
      countNext n (_ : xs) = (n + 1, xs)

countStringEscaped :: String -> Int
countStringEscaped s
    | head s == '"' && last s == '"' =
        countEscaped s - 2
    | otherwise = error "bad string"

solve1 :: [String] -> Int
solve1 stuff = sum $ map diff stuff
  where diff s = length s - countStringEscaped s

-- solve2 :: [String] -> Int
solve2 input = sum $ map diff input
  where diff s = (length $ show s) - length s

main :: IO()
main = mainWrapper "day8" solve1 solve2
