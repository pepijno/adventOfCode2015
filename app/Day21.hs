module Main where

import Lib
import Data.List
import Data.Function
import Control.Arrow

data Player = Player
  { hitPoints :: Int
  , damage :: Int
  , armor :: Int
  } deriving (Show)

boss :: Player
boss = Player { hitPoints = 109, damage = 8, armor = 2 }

doDamage :: Player -> Player -> Int
doDamage from to = max 1 (damage from - armor to)

subtractHP :: Player -> Int -> Player
subtractHP p d = Player { hitPoints = max 0 (hitPoints p - d), damage = damage p, armor = armor p }

fight :: Player -> Player -> Bool
fight Player{hitPoints=0} _ = False
fight player other = not $ fight (subtractHP other d) player
  where d = doDamage player other

damageCost :: Int -> Int
damageCost 0 = 0
damageCost 4 = 8
damageCost 5 = 10
damageCost 6 = 25
damageCost 7 = 40
damageCost 8 = 65
damageCost 9 = 90
damageCost 10 = 124
damageCost 11 = 174

armorCost :: Int -> Int
armorCost 0 = 0
armorCost 1 = 13
armorCost 2 = 31
armorCost 3 = 51
armorCost 4 = 71
armorCost 5 = 93
armorCost 6 = 115
armorCost 7 = 142
armorCost 8 = 182

playerToCost :: Player -> Int
playerToCost (Player{damage=a,armor=b}) = damageCost a + armorCost b

solve1 :: [String] -> Int
solve1 _ = minimum $ map (playerToCost . fst) $ filter (snd) $ map (\x -> (x, fight x boss)) players
  where players = map (\(a, b) -> Player { hitPoints = 100, damage = a, armor = b }) $ [(a, b) | a <- [4..11], b <- [0..8]]

damageCostMax :: Int -> Int
damageCostMax 4 = 8
damageCostMax 5 = 8 + 25
damageCostMax 6 = 8 + 50
damageCostMax 7 = 8 + 100
damageCostMax 8 = 10 + 100
damageCostMax 9 = 25 + 100
damageCostMax 10 = 40 + 100
damageCostMax 11 = 74 + 100

armorCostMax :: Int -> Int
armorCostMax 0 = 0
armorCostMax 1 = 20
armorCostMax 2 = 40
armorCostMax 3 = 80
armorCostMax 4 = 13 + 80
armorCostMax 5 = 31 + 80
armorCostMax 6 = 53 + 80
armorCostMax 7 = 75 + 80
armorCostMax 8 = 100 + 82

playerToCostMax :: Player -> Int
playerToCostMax (Player{damage=a,armor=b}) = damageCostMax a + armorCostMax b

solve2 :: [String] -> Int
solve2 _ = maximum $ map (playerToCostMax . fst) $ filter (not . snd) $ map (\x -> (x, fight x boss)) players
  where players = map (\(a, b) -> Player { hitPoints = 100, damage = a, armor = b }) $ [(a, b) | a <- [4..11], b <- [0..8]]

main :: IO()
main = mainWrapper "day21" solve1 solve2
