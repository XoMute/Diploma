{-# LANGUAGE FlexibleInstances #-} -- todo: read about this

module Parser
  -- (
  -- Json(JsonNull,
  --      JsonBool,
  --      JsonString,
  --      JsonNumber,
  --      JsonArray,
  --      JsonObject),
  -- Parser,
  -- parse,
  -- jsonParser
  -- ) -- TODO: uncomment
where

import Control.Applicative
import Control.Monad (guard)
import Data.Char (isDigit)

type Position = (Int, Int)
data Input = Input { pos :: Position, input :: String } deriving (Show, Eq) -- todo: do I need Eq?

-- constructor
inputFrom :: String -> Input
inputFrom = Input (1, 1)

-- Get first character from input if it is present
nextChar :: Input -> Maybe (Char, Input)
nextChar (Input _ "") = Nothing
nextChar (Input (posX, posY) (x:xs))
  | x == '\n' = Just (x, Input (1, posY + 1) xs)
  | otherwise = Just (x, Input (posX + 1, posY) xs)

data ParseError = ParseError Position String deriving (Show)

newtype Parser a = Parser { parse :: Input -> Either ParseError (a, Input) }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (v, input') <- p input
      Right (f v, input')

instance Applicative Parser where
  pure v = Parser $ \input -> Right (v, input)

  (Parser pf) <*> (Parser p) =
    Parser $ \input -> do
      (f, rest) <- pf input
      (v, rest') <- p rest
      pure (f v, rest')

-- todo: do I need this code?
instance Alternative (Either ParseError) where
  empty = Left $ ParseError (0, 0) ""
  Left _ <|> r = r
  l <|> _ = l

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    p1 input <|> p2 input

instance Monad Parser where
  return a = Parser $ \input -> pure (a, input)
  p >>= f = Parser $ \input ->
    case parse p input of
      Right (p', cs) -> parse (f p') cs
      Left error -> Left error
  fail message = Parser $ \input -> Left $ ParseError (pos input) message

-- check if input is left and throw an error if it is
failIfNotFinished :: Parser a -> Parser a
failIfNotFinished p = Parser $ \inp ->
  let result = parse p inp in
    case result of
      Right (_, inp') ->
        if null (input inp')
        then result
        else error $ "Could not parse input to the end: " ++ (input inp')
      otherwise -> result

oneOf :: [Parser a] -> Parser a
oneOf = foldl (<|>) empty

manySepBy :: Parser a -> Parser b -> Parser [a]
manySepBy p delim = manySepBy1 p delim <|> empty

manySepBy1 :: Parser a -> Parser b -> Parser [a]
manySepBy1 p delim = do
  v  <- p -- first element parsed
  vs <- many $ do {delim; p} -- parse many (0 or more) of delimiter, followed by element
  return (v:vs)

quote :: Char -> String
quote c = ['\'', c, '\'']

-- 1. Get char from Input
-- 2. If return is Nothing - return EOF error
-- 3. Else check if chars are equal and return Left or Right respectively
char :: Char -> Parser Char
char c = Parser f
  where
    f input =
      case nextChar input of
        Nothing -> Left $
          ParseError (pos input) "EOF"
        Just (x, input') ->
          if c == x then Right (c, input')
          else Left $
               ParseError (pos input) ((quote c) ++ " expected, but found " ++ (quote x))

whitespace :: Parser Char
whitespace = oneOf [char '\n', char '\r', char '\t', char ' ']

ws :: Parser String
ws = many whitespace

ws1 :: Parser String
ws1 = some whitespace

character :: Parser Char
character =
  parseWhen
    (\c -> c /= '"' && c /= '\\') <|>
  escape

escape :: Parser Char
escape = oneOf [
  ('"'  <$ string "\\\""),
  ('\\' <$ string "\\\\"),
  ('/'  <$ string "\\/"),
  ('\b' <$ string "\\b"),
  ('\f' <$ string "\\f"),
  ('\n' <$ string "\\n"),
  ('\r' <$ string "\\r"),
  ('\t' <$ string "\\t")]--  <|>
--   (string "\\u" *> escapeUnicode)

-- escapeUnicode :: parserChar
-- escapeUnicode = 

string :: String -> Parser String
string "" = Parser $ \input -> Right ("", input)
string (c:cs) = do
  char c
  string cs
  return (c:cs)

-- constructs Double from given parsed parts
constructDouble :: Integer -> Integer -> Double -> Integer -> Double
constructDouble sign integral decimal exponent =
  fromIntegral sign * (fromIntegral integral + decimal) * (10 ^^ exponent)

-- TODO: save info about number to recreate it successfully
double :: Parser Double
double = let minus = (-1) <$ char '-'
             plus = 1 <$ char '+'
             digits = some $ parseWhen isDigit
             e = char 'e' <|> char 'E'
         in do
             sign <- minus <|> pure 1
             integral <- read <$> digits
             -- append "0." to digits and read result as double
             decimal <- read <$> (("0." ++) <$> (char '.' *> digits)) <|> pure 0
             -- multiply read number by 1 or -1 (depending on sign or it's absence)
             exponent <- (e *> ((*) <$> (plus <|> minus <|> pure 1) <*> (read <$> digits)) <|> pure 0)
             return (constructDouble sign integral decimal exponent)

between :: Parser a -> Parser b -> Parser c -> Parser a
between p left right = do
  left
  res <- p
  right
  return res

-- parses character only if it matches predicate
parseWhen :: (Char -> Bool) -> Parser Char
parseWhen pred = Parser $ \input ->
  case nextChar input of
    Nothing -> Left $
      ParseError (pos input) "EOF"
    Just (x, input') ->
      if pred x then Right (x, input')
      else Left $
           ParseError (pos input) (quote x)

