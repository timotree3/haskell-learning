{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Use first" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Lib
  ( Parser (..),
    satisfy,
    token,
    char,
    string,
    jsonValue,
  )
where

import Control.Applicative
import Data.Foldable (asum)
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- TODO: actually have errors
newtype Parser s a = Parser {runParser :: s -> Maybe (a, s)}

instance Functor (Parser s) where
  fmap f (Parser p) = Parser $ \input -> do
    (x, rest) <- p input
    pure (f x, rest)

instance Applicative (Parser s) where
  pure x = Parser $ \input -> Just (x, input)
  Parser p <*> Parser q = Parser $ \input -> do
    (f, rest) <- p input
    (x, rest') <- q rest
    pure (f x, rest')

instance Monad (Parser s) where
  Parser p >>= f = Parser $ \input -> do
    (x, rest) <- p input
    runParser (f x) rest

instance Alternative (Parser s) where
  empty = Parser $ const Nothing
  Parser p <|> Parser q = Parser $ \input -> case (p input, q input) of
    (Just x, _) -> Just x
    (_, Just x) -> Just x
    _ -> Nothing

class Stream s where
  type Token s
  uncons :: s -> Maybe (Token s, s)

instance Stream String where
  type Token String = Char
  uncons = L.uncons

predToMaybe :: (a -> Bool) -> a -> Maybe a
predToMaybe f x = if f x then Just x else Nothing

token :: (Stream s) => (Token s -> Maybe a) -> Parser s a
token f = Parser $ \input -> do
  (x, xs) <- uncons input
  y <- f x
  pure (y, xs)

satisfy :: (Stream s) => (Token s -> Bool) -> Parser s (Token s)
satisfy f = token $ predToMaybe f

takeWhileP :: (Stream s) => (Token s -> Bool) -> Parser s [Token s]
takeWhileP = many . satisfy

char :: Char -> Parser String Char
char c = satisfy (== c)

string :: String -> Parser String String
string = traverse char

data JsonValue
  = Object (Map String JsonValue)
  | Array [JsonValue]
  | String String
  | Number Double
  | Boolean Bool
  | Null
  deriving (Show)

jsonValue :: Parser String JsonValue
jsonValue = ignoreWhitespaceAround $ object <|> array <|> stringLit <|> number <|> boolean <|> nullLit

ignoreWhitespaceAround :: Parser String a -> Parser String a
ignoreWhitespaceAround x = many whitespace *> x <* many whitespace

whitespace :: Parser String Char
whitespace = asum $ char <$> ['\n', '\t', ' ', '\r']

nullLit :: Parser String JsonValue
nullLit = Null <$ string "null"

boolean :: Parser String JsonValue
boolean = Boolean <$> (true <|> false)
  where
    true = True <$ string "true"
    false = False <$ string "false"

number :: Parser String JsonValue
number = Number <$> (decimal <|> integral)

decimal :: Parser String Double
decimal = signed ((+) <$> natural <*> fractionalPart)

fractionalPart :: Parser String Double
fractionalPart = char '.' *> (foldr prependDecimal 0 <$> some digit)

prependDecimal :: Int -> Double -> Double
prependDecimal d n = (n + fromIntegral d) / 10

integral :: Parser String Double
integral = signed natural

signed :: Parser String Double -> Parser String Double
signed x = ((0 -) <$> (char '-' *> x)) <|> x

natural :: Parser String Double
natural = fromIntegral . fromDigits <$> some digit

fromDigits :: [Int] -> Int
fromDigits = L.foldl' appendDigit 0

digit :: Parser String Int
digit = asum (digitLit <$> [0 .. 9])
  where
    digitLit n = n <$ char (showDigit n)

showDigit :: Int -> Char
showDigit n = fromSingleton (show n)
  where
    fromSingleton [x] = x
    fromSingleton _ = undefined

appendDigit :: Int -> Int -> Int
appendDigit x d = x * 10 + d

stringLit :: Parser String JsonValue
stringLit = String <$> stringLitRaw

stringLitRaw :: Parser String String
stringLitRaw = char '"' *> substituteUntil backslashEscape (char '"')

backslashEscape :: Parser String Char
backslashEscape = char '\\' *> (char '"' <|> char '\\')

substituteUntil :: Stream s => Parser s (Token s) -> Parser s a -> Parser s [Token s]
substituteUntil substitution = repeatUntil (substitution <|> anyToken)

anyToken :: Stream s => Parser s (Token s)
anyToken = Parser uncons

repeatUntil :: Stream s => Parser s a -> Parser s b -> Parser s [a]
repeatUntil x y = ungreedy x (id <$ y)

-- repeatUntil x y = ([] <$ y) <|> ((:) <$> x <*> repeatUntil x y)

-- `ungreedy x y` is equivalent to `(\xs f -> f xs) <$> many x <*> y`, except that it accepts the fewest possible occurences of x
ungreedy :: Stream s => Parser s a -> Parser s ([a] -> b) -> Parser s b
ungreedy x y = (\(f, xs) -> f xs) <$> go
  where
    go = (,[]) <$> y <|> (\x (f, xs) -> (f, x : xs)) <$> x <*> go

array :: Parser String JsonValue
array = Array <$> (char '[' *> separated jsonValue (char ',') <* char ']')

terminated :: Stream s => Parser s a -> Parser s b -> Parser s [a]
terminated x sep = many (x <* sep)

separated :: Stream s => Parser s a -> Parser s b -> Parser s [a]
separated x sep = ((:) <$> x <*> prefixed x sep) <|> pure []
  where
    prefixed x sep = many (sep *> x)

object :: Parser String JsonValue
object = Object <$> (char '{' *> (Map.fromList <$> separated kvPair (char ',')) <* char '}')
  where
    kvPair :: Parser String (String, JsonValue)
    kvPair = (,) <$> ignoreWhitespaceAround stringLitRaw <*> (char ':' *> jsonValue)