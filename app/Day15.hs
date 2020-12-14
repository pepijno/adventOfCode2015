module Main where

import Lib

data Ingredient = Ingredient
  { capacity :: Int
  , durability :: Int
  , flavor :: Int
  , texture :: Int
  , calories :: Int
  } deriving (Show)

parseIngredient :: Parser Ingredient
parseIngredient = Ingredient <$>
  (stringLiteral *> string " capacity " *> integer) <*>
    (string ", durability " *> integer) <*>
      (string ", flavor " *> integer) <*>
        (string ", texture " *> integer) <*>
          (string ", calories " *> integer)

properties :: (Ingredient -> Int) -> [(Ingredient, Int)] -> Int
properties f = max 0 . foldl (\y (a, b) -> y + b * f a) 0

totals :: [(Ingredient, Int)] -> Int
totals xs = product [(properties capacity xs), (properties durability xs), (properties flavor xs), (properties texture xs)]

solve1 :: [String] -> Int
solve1 xs = maximum $ map (totals . zip is) tsps
  where tsps = [[a, b, c, d] | a <- [0..100], b <- [0..100], c <- [0..100], d <- [0..100], a + b + c + d == 100]
        is = map (unsafeParse parseIngredient) xs

totals2 :: [(Ingredient, Int)] -> Int
totals2 xs = if (properties calories xs) == 500
                then product [(properties capacity xs), (properties durability xs), (properties flavor xs), (properties texture xs)]
                else 0

solve2 :: [String] -> Int
solve2 xs = maximum $ map (totals2 . zip is) tsps
  where tsps = [[a, b, c, d] | a <- [0..100], b <- [0..100], c <- [0..100], d <- [0..100], a + b + c + d == 100]
        is = map (unsafeParse parseIngredient) xs

main :: IO()
main = mainWrapper "day15" solve1 solve2
