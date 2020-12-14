{-# language TypeFamilies #-}
module Main where

import Lib
import Data.Functor.Compose (Compose(..))
import Data.Vector (Vector, (!), generate)
import Data.Bool (bool)
import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Functor.Identity (Identity(..))
import Control.Comonad.Representable.Store (Store(..), StoreT(..), store, experiment)
import Control.Comonad (Comonad(..))
import qualified Data.Map.Strict as M
import Data.Monoid (Sum, getSum)

type Coord = (Int, Int)
type Grid = Store (Compose Vector Vector) Bool
type Rule = Grid -> Bool

instance Distributive Vector where
  distribute = distributeRep

instance Representable Vector where
  type Rep Vector = Int
  index v i = v ! i
  tabulate = generate gridSize

gridSize :: Int
gridSize = 100

neighbourCoords :: Coord -> [Coord]
neighbourCoords (a, b) = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0), (a + x) >= 0, (a + x) < gridSize, (b + y) >= 0, (b + y) < gridSize]

addCoords :: Coord -> Coord -> Coord
addCoords (x, y) (x', y') = (x + x', y + y')

basicRule :: Rule
basicRule g = numNeighboursAlive == 3 || (alive && numNeighboursAlive == 2)
  where
    alive = extract g
    neighbours = experiment (\s -> at (neighbourCoords s) s) g
    numNeighboursAlive = length (filter id neighbours)

step :: Rule -> Grid -> Grid
step = extend

render :: Grid -> Int
render (StoreT (Identity (Compose g)) _) = getSum $ foldMap ((+ 0) . foldMap (bool 0 1)) g

mkGrid :: [Coord] -> Grid
mkGrid xs = store (`elem` xs) (0, 0)

at :: [Coord] -> Coord -> [Coord]
coords `at` origin = map (addCoords origin) coords

parseGrid :: [String] -> [(Int, Int)]
parseGrid xs = M.keys $ M.filter (=='#') m
  where m = M.fromList [((row, col), b) | (row, line) <- zip [0..] xs, (col, b) <- zip [0..] line]

solve1 :: [String] -> Int
solve1 = render . head . drop 100 . iterate (step basicRule) . mkGrid . parseGrid

extendedRule :: Rule
extendedRule g = numNeighboursAlive == 3 || (alive && numNeighboursAlive == 2)
  where
    alive = extract g
    neighbours = experiment (\s -> at (neighbourCoords s) s) g
    numNeighboursAlive = if length neighbours == 3 then 3 else length (filter id neighbours)

solve2 :: [String] -> Int
solve2 = render . head . drop 100 . iterate (step extendedRule) . mkGrid . parseGrid

main :: IO()
main = do
  contents <- lines <$> readFile ("./inputs/day18.txt")
  print $ solve1 $ contents
  print $ solve2 $ contents
