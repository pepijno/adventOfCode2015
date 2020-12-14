module Main where

import Lib
import Data.List
import Data.Function
import Control.Arrow

data Reindeer = Reindeer 
  { name :: String
  , speed :: Int
  , duration :: Int
  , rest :: Int
  } deriving (Show)

parseReindeer :: Parser Reindeer
parseReindeer = Reindeer <$> stringLiteral <* string " can fly " <*> integer <* string " km/s for " <*> integer <* string " seconds, but then must rest for " <*> integer <* string " seconds."

reindeerTravelled :: Int -> Reindeer -> Int
reindeerTravelled sec rd
  | r < duration rd = r * speed rd + s
  | otherwise = duration rd * speed rd + s
  where m = rest rd + duration rd
        (d, r) = sec `quotRem` m
        s = d * speed rd * duration rd

solve1 :: [String] -> Int
solve1 = maximum . map (reindeerTravelled 2503 . unsafeParse parseReindeer)

allTravelled :: [Reindeer] -> Int -> [(String, Int)]
allTravelled rds sec = map (name &&& reindeerTravelled sec) rds

winningReindeers :: [(String, Int)] -> [String]
winningReindeers xs = map fst . filter ((==) w . snd) $ xs
  where w = snd . head . sortBy ((flip compare) `on` snd) $ xs

countWinner :: [String] -> Int
countWinner = maximum . map length . group . sort

solve2 :: [String] -> Int
solve2 xs = countWinner . concat . map (winningReindeers . allTravelled rds) $ [1..2503]
  where rds = map (unsafeParse parseReindeer) xs

main :: IO()
main = mainWrapper "day14" solve1 solve2
