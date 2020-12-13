module Main where

import Lib
import Data.List
import Data.Tuple
import Control.Applicative
import qualified Data.Map as M

parseUnits :: Parser Int
parseUnits = (*) <$> (pure (-1) <* string "lose" <|> pure 1 <* string "gain") <*> (char ' ' *> integer <* string " happiness units")

parseLine :: Parser ((String, String), Int)
parseLine = (\a b c -> ((a, c), b)) <$> (stringLiteral <* string " would ") <*> parseUnits <*> (string " by sitting next to " *> stringLiteralUntil '.' <* char '.')

generateSeats :: [String] -> [[String]]
generateSeats [] = []
generateSeats (x:xs) = map (x:) $ permutations xs

calcHappiness :: M.Map (String, String) Int -> [String] -> Int
calcHappiness m xs = sum $ map (\x -> f (swap x) + f x) $ zip xs ((tail xs)++[head xs])
  where f x = M.findWithDefault 0 x m

solve1 :: [String] -> Int
solve1 xs = maximum $ map (calcHappiness m) $ generateSeats $ ps
  where m = M.fromList . map (unsafeParse parseLine) $ xs
        ps = nub $ map fst $ M.keys m

solve2 :: [String] -> Int
solve2 xs = maximum $ map (calcHappiness m') $ generateSeats $ ps'
  where m = M.fromList . map (unsafeParse parseLine) $ xs
        ps = nub $ map fst $ M.keys m
        ps' = "yourself":ps
        m' = M.union m $ M.fromList $ concat $ map (\x -> [(("yourself", x), 0), ((x, "yourself"), 0)]) ps

main :: IO()
main = mainWrapper "day13" solve1 solve2
