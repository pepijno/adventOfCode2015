module Main where

import Lib
import Control.Applicative
import qualified Data.Map.Strict as M

data JsonValue = JsonNumber Int | JsonString String | JsonArray [JsonValue] | JsonObject (M.Map String JsonValue) deriving (Show)

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber <$> integer

jsonString :: Parser JsonValue
jsonString = JsonString <$> (char '"' *> splitBy (/= '"') <* char '"')

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (char '[' *> whiteSpace *> elements <* whiteSpace <* char ']')
  where elements = sepBy (whiteSpace *> char ',' <* whiteSpace) jsonValue

jsonObject :: Parser JsonValue
jsonObject = (JsonObject . M.fromList) <$> (char '{' *> whiteSpace *> sepBy (whiteSpace *> char ',' <* whiteSpace) pair <* whiteSpace <* char '}')
  where pair = (,) <$> (char '"' *> splitBy (/= '"') <* char '"') <*> (whiteSpace *> char ':' *> whiteSpace *> jsonValue)

jsonValue :: Parser JsonValue
jsonValue = jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

countInts :: JsonValue -> Int
countInts (JsonNumber a) = a
countInts (JsonString _) = 0
countInts (JsonArray xs) = sum $ map countInts xs
countInts (JsonObject m) = sum $ map countInts $ M.elems m

solve1 :: [String] -> Int
solve1 = countInts . unsafeParse jsonValue . head

countInts2 :: JsonValue -> Int
countInts2 (JsonNumber a) = a
countInts2 (JsonString _) = 0
countInts2 (JsonArray xs) = sum $ map countInts2 xs
countInts2 (JsonObject m) = if length [x | (JsonString x) <- es, x == "red"] > 0
                               then 0
                               else sum $ map countInts2 $ es
                                 where es = M.elems m

solve2 :: [String] -> Int
solve2 = countInts2 . unsafeParse jsonValue . head

main :: IO()
main = mainWrapper "day12" solve1 solve2
