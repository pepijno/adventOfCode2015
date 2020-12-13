module Main where

import Lib
import Data.List
import qualified Data.Map as M

type Graph = M.Map String (M.Map String Int)

parseConnection :: Parser (String, String, Int)
parseConnection = (,,) <$> stringLiteral <*> (string " to " *> stringLiteral <* string " = ") <*> integer

addConnection :: Graph -> (String, String, Int) -> Graph
addConnection g (a, b, i) = M.insert b (M.insert a i b') $ M.insert a (M.insert b i a') g
  where a' = M.findWithDefault M.empty a g
        b' = M.findWithDefault M.empty b g

makeGraph :: [(String, String, Int)] -> Graph
makeGraph input = foldl addConnection M.empty input

makeWalk :: Graph-> [[String]]
makeWalk g = permutations $ M.keys g

walkLength :: Graph -> [String] -> Int
walkLength g walk = sum $ map (\(from, to) -> M.findWithDefault 0 to $ M.findWithDefault M.empty from g) $ zip walk (tail walk)

parseGraph :: [String] -> Graph
parseGraph = makeGraph . map (unsafeParse parseConnection)

solve1 :: [String] -> Int
solve1 xs = minimum . map (walkLength g) . makeWalk $ g
  where g = parseGraph xs

solve2 :: [String] -> Int
solve2 xs = maximum . map (walkLength g) . makeWalk $ g
  where g = parseGraph xs

main :: IO()
main = mainWrapper "day9" solve1 solve2
