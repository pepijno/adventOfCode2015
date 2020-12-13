module Lib (
  mainWrapper,
  inRange,
  groupPairs,

  Parser,
  parse,
  unsafeParse,
  char,
  anyChar,
  string,
  integer,
  stringLiteral,
  stringLiteralUntil,
  whiteSpace,
  letters,
  splitBy,
  notNull
) where

import Data.Char
import Data.Maybe
import Control.Applicative
import Data.List.Split

mainWrapper :: (Show a, Show b) => String -> ([String] -> a) -> ([String] -> b) -> IO()
mainWrapper file f1 f2 = do
  contents <- lines <$> readFile ("./inputs/" ++ file ++ ".txt")
  print $ f1 $ contents
  print $ f2 $ contents

inRange :: (Ord a) => a -> a -> a -> Bool
inRange v min max = min <= v && v <= max

groupPairs :: [String] -> [[String]]
groupPairs = splitOn [""]

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, rest) <- p input
    Just (f x, rest)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (Parser a) <*> (Parser b) = Parser $ \input -> do
    (f, rest) <- a input
    (x, rest') <- b rest
    Just (f x, rest')

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser a) <|> (Parser b) = Parser $ \input -> (a input) <|> (b input)

unsafeParse :: Parser a -> String -> a
unsafeParse p = fst . fromJust . parse p

char :: Char -> Parser Char
char c = Parser f
  where f (x:rest)
          | x == c = Just (x, rest)
          | otherwise = Nothing
        f [] = Nothing

anyChar :: Parser Char
anyChar = Parser f
  where f (x:rest) = Just (x, rest)
        f [] = Nothing

string :: String -> Parser String
string = sequenceA . map char

splitBy :: (Char -> Bool) -> Parser String
splitBy f = Parser $ \input -> Just $ span f input

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
  (input', rest) <- p input
  if null input' then Nothing else Just (input', rest)

integer :: Parser Int
integer = read <$> notNull (splitBy (\x -> isDigit x || x == '-'))

whiteSpace :: Parser String
whiteSpace = splitBy isSpace

letters :: Parser String
letters = splitBy isLetter

stringLiteral :: Parser String
stringLiteral = splitBy (not . isSpace)

stringLiteralUntil :: Char -> Parser String
stringLiteralUntil c = splitBy (\x -> not (isSpace x) && x /= c)
