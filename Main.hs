module Main where

-- TODO: MAKE SURE IT'S NOT COPYWRITED
-- https://www.cmi.ac.in/~spsuresh/teaching/prgh15/papers/monadic-parsing.pdf

-- TODO: change Maybe to Either, and provide user with error messages

import Control.Applicative
import Control.Monad (guard)

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (v, input') <- p input
      Just (f v, input')

instance Applicative Parser where
  pure v = Parser $ \input -> Just (v, input)

  (Parser pf) <*> (Parser p) =
    Parser $ \input -> do
      (f, rest) <- pf input
      (v, rest') <- p rest
      pure $ (f v, rest')

instance Alternative Parser where
  empty = Parser $ const Nothing

  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    (p1 input) <|> (p2 input)

instance Monad Parser where
  return a = Parser $ \input -> pure (a, input)
  p >>= f = Parser $ \input ->
    case parse p input of
      Just (p', cs) -> parse (f p') cs
      _ -> Nothing

oneOf :: [Parser a] -> Parser a
oneOf = foldl (<|>) empty

manySepBy :: Parser a -> Parser b -> Parser [a]
manySepBy p delim = manySepBy1 p delim <|> empty

manySepBy1 :: Parser a -> Parser b -> Parser [a]
manySepBy1 p delim = do
  v  <- p -- first element parsed
  vs <- many $ do {delim; p} -- parse many (0 or more) of delimiter, followed by element
  return (v:vs)

anyChar :: Parser Char
anyChar = Parser $ \x ->
  case x of
    (c:rest)  -> Just (c, rest)
    ""        -> Nothing

char :: Char -> Parser Char
char c = anyChar `when` (== c)

ws :: Parser String
ws = many $ anyChar `when` isSpace
  where
    isSpace c = any (c ==) " \n\r\t"
-- ws = many $ oneOf [char '\n', char '\r', char '\t', char ' '] -- TODO: Choose most effective

character :: Parser Char
character =
  anyChar `unless` (\c -> c == '"' || c == '\\') <|>
  escape

escape :: Parser Char
escape =
  ('"'  <$ string "\\\"") <|>
  ('\\' <$ string "\\\\") <|>
  ('/'  <$ string "\\/")  <|>
  ('\b' <$ string "\\b")  <|>
  ('\f' <$ string "\\f")  <|>
  ('\n' <$ string "\\n")  <|>
  ('\r' <$ string "\\r")  <|>
  ('\t' <$ string "\\t")
      -- TODO: unicode

string :: String -> Parser String
string "" = Parser $ \input -> Just("", input)
string (c:cs) = do
  char c
  string cs
  return (c:cs)

-- Explanation: we have a list with pairs [(0, '0'), (1, '1')] etc.
-- We map it with function, that should create parser for each that char, and return int of that char.
-- When some function is mapped to a parser, it will execute on parsed value AFTER parsing. So,
-- when whe map 'const v' to parser, it means, that parser will HAVE TO parse some character, and then
-- instead of that character it will return corresponding digit no matter what it's input is, as long
-- as it was parsed without error. oneOf just folds all parsers with alternative <|>, which means that
-- from list of parsers we get parser of 'lists' of chars
digit :: Parser Integer
digit = oneOf $ (\(v, c) -> const v <$> char c) <$> zip [0..] ['0'..'9']

unsignedInteger :: Parser Integer
unsignedInteger = foldl (\acc item -> item + acc * 10) 0 <$> some digit

negativeInteger :: Parser Integer
negativeInteger = (*) (-1) <$> (char '-' *> unsignedInteger)

integer :: Parser Integer
integer = unsignedInteger <|> negativeInteger

element :: Parser Json
element = ws *> jsonValue <* ws

between :: Parser a -> Parser b -> Parser c -> Parser a
between p left right = left *> p <* right

-- modifies parser with given predicate (parse when something)
when :: Parser a -> (a -> Bool) -> Parser a
when p pred = Parser $ \input -> do
  (v, rest) <- parse p input
  guard $ pred v
  return (v, rest)

-- parse unless something
unless :: Parser a -> (a -> Bool) -> Parser a
unless p pred = when p (not . pred)

data Json
  = JsonNull
  | JsonBool Bool
  | JsonString String
  | JsonNumber Integer
  | JsonArray [Json]
  | JsonObject [(String, Json)]
  deriving (Show, Eq)

jsonNull :: Parser Json
jsonNull = do
  string "null"
  return JsonNull

jsonTrue :: Parser Json
jsonTrue = do
  string "true"
  return (JsonBool True)

jsonFalse :: Parser Json
jsonFalse = do
  string "false"
  return (JsonBool False)

jsonString :: Parser Json
jsonString = JsonString <$> between (many character) (char '"') (char '"')

jsonNumber :: Parser Json
jsonNumber = JsonNumber <$> integer

jsonArray :: Parser Json
jsonArray = JsonArray <$>
  (between (const [] <$> ws) (char '[') (char ']')
   <|> between elements (char '[') (char ']'))
  where
    elements = manySepBy element (char ',')

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
    members = manySepBy member (char ',')


jsonValue :: Parser Json
jsonValue =
  jsonObject <|>
  jsonArray  <|>
  jsonNumber <|>
  jsonString <|>
  jsonTrue   <|>
  jsonFalse  <|>
  jsonNull

-- Usage: parse jsonParser *input*
jsonParser :: Parser Json
jsonParser = ws *> jsonValue <* ws

main :: IO ()
main = do
  jsonStr <- readFile "./test.json"
  let result = parse jsonParser jsonStr
  print result

