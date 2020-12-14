module Main where

import Lib
import Control.Applicative
import qualified Data.Map.Strict as M

data Aunt = Aunt
  { id :: Int
  , things :: M.Map String Int
  } deriving (Show)

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

parseAunt :: Parser Aunt
parseAunt = Aunt <$> (string "Sue " *> integer <* string ": ") <*> (M.fromList <$> sepBy (string ", ") pair)
  where pair = (,) <$> (splitBy (/=':') <* string ": ") <*> integer

myProps :: M.Map String Int
myProps = M.fromList [("children", 3), ("cats", 7), ("samoyeds", 2), ("pomeranians", 3), ("akitas", 0), ("vizslas", 0), ("goldfish", 5), ("trees", 3), ("cars", 2), ("perfumes", 1)]

isMyAunt :: Aunt -> Bool
isMyAunt a = (things a) == (M.differenceWith (\a b -> if a /= b then Nothing else Just a) (things a) myProps)

solve1 :: [String] -> Int
solve1 = Main.id . head . filter isMyAunt . map (unsafeParse parseAunt)

isMyAunt2 :: Aunt -> Bool
isMyAunt2 a = (things a) == (M.differenceWithKey f (things a) myProps)
  where f k a b
          | k `elem` ["cats", "trees"] = if a > b then Just a else Nothing
          | k `elem` ["pomeranians", "goldfish"] = if a < b then Just a else Nothing
          | otherwise = if a == b then Just a else Nothing

solve2 :: [String] -> Int
solve2 = Main.id . head . filter isMyAunt2 . map (unsafeParse parseAunt)

main :: IO()
main = mainWrapper "day16" solve1 solve2
