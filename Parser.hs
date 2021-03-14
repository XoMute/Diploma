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
-- import Numeric
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

---------------------------- Pos Message
data ParseError = ParseError Position String
  deriving (Show)

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

-- todo: add error message
oneOf :: String -> [Parser a] -> Parser a
oneOf desc = foldl (<|>) err
  where err = fail $ "Couldn't parse " ++ desc

-- todo: add error message
manySepBy :: String -> Parser a -> Parser b -> Parser [a]
manySepBy desc p delim = manySepBy1 p delim <|> err
  where err = fail $ "Couldn't parse " ++ desc

manySepBy1 :: Parser a -> Parser b -> Parser [a]
manySepBy1 p delim = do
  v  <- p -- first element parsed
  vs <- many $ do {delim; p} -- parse many (0 or more) of delimiter, followed by element
  return (v:vs)

errorMessage :: String -> String -> String
errorMessage expected found = expected ++ " expected, but " ++ found ++ " found"

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
          ParseError (pos input) (errorMessage (quote c) "EOF")
        Just (x, input') ->
          if c == x then Right (c, input')
          else Left $
               ParseError (pos input) (errorMessage (quote c) (quote x))

ws :: Parser String
ws = many $ oneOf "whitespace character"[char '\n', char '\r', char '\t', char ' ']

character :: Parser Char
character =
  parseWhen
    "Non-special character"
    (\c -> c /= '"' && c /= '\\') <|>
  escape

escape :: Parser Char
escape = oneOf "escape character" [
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

-- TODO: do I really need to parse doubles to numbers? I can just left them as strings
--       or maybe it will be handled in generator (it may be problematic because
--       if numbers were written in exponential form - it will be lost)
-- TODO: check if error will happen if 'e' is present without any digits
double :: Parser Double
double = let minus = (-1) <$ char '-'
             plus = 1 <$ char '+'
             digits = some $ parseWhen "Digit" isDigit
             e = char 'e' <|> char 'E'
         in do
             sign <- minus <|> pure 1 -- pure will return given value without reading it from input
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
parseWhen :: String -> (Char -> Bool) -> Parser Char
parseWhen description pred = Parser $ \input ->
  case nextChar input of
    Nothing -> Left $
      ParseError (pos input) (errorMessage description "EOF")
    Just (x, input') ->
      if pred x then Right (x, input')
      else Left $
           ParseError (pos input) (errorMessage description (quote x))

data Json
  = JsonNull
  | JsonBool Bool
  | JsonString String
  | JsonNumber Double
  | JsonArray [Json]
  | JsonObject [(String, Json)]
  deriving (Show, Eq)

jsonNull :: Parser Json
jsonNull = JsonNull <$ string "null"

jsonTrue :: Parser Json
jsonTrue = (JsonBool True) <$ string "true"

jsonFalse :: Parser Json
jsonFalse = (JsonBool False) <$ string "false"

jsonString :: Parser Json
jsonString = JsonString <$> between (many character) (char '"') (char '"')

jsonNumber :: Parser Json
jsonNumber = JsonNumber <$> double

element :: Parser Json
element = ws *> jsonValue <* ws

jsonArray :: Parser Json
jsonArray = JsonArray <$>
  (between (const [] <$> ws) (char '[') (char ']')
   <|> between elements (char '[') (char ']'))
  where
    elements = manySepBy "JSON element" element (char ',')

jsonObject :: Parser Json
jsonObject = JsonObject <$>
  (between (const [] <$> ws) (char '{') (char '}')
   <|> between members (char '{') (char '}'))
  where
    member = do
      ws
      jsonKey <- jsonString
      ws
      char ':'
      value <- element
      case jsonKey of
        JsonString key -> return (key, value)
        _ -> empty
    members = manySepBy "JSON object member" member (char ',') -- todo: change error message

jsonValue :: Parser Json
jsonValue = oneOf "JSON element" [
  jsonObject,
  jsonArray,
  jsonNumber,
  jsonString,
  jsonTrue,
  jsonFalse,
  jsonNull]

-- Usage: parse jsonParser *input*
jsonParser :: Parser Json
jsonParser = element

