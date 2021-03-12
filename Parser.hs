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
  Left _ <|> e = e
  e <|> _ = e

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

oneOf :: [Parser a] -> Parser a
oneOf = foldl (<|>) empty

manySepBy :: Parser a -> Parser b -> Parser [a]
manySepBy p delim = manySepBy1 p delim <|> empty

manySepBy1 :: Parser a -> Parser b -> Parser [a]
manySepBy1 p delim = do
  v  <- p -- first element parsed
  vs <- many $ do {delim; p} -- parse many (0 or more) of delimiter, followed by element
  return (v:vs)

-- anyChar :: Parser Char
-- anyChar = Parser $ \x ->
--   case x of
--     (c:rest)  -> Right (c, rest)
--     ""        -> Left (ParseError 0 "") -- todo: fix

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
ws = many $ oneOf [char '\n', char '\r', char '\t', char ' ']

character :: Parser Char
character =
  escape <|>
  parseWhen
    "Non-special character"
    (\c -> c == '"' || c == '\\')

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
string "" = Parser $ \input -> Right ("", input)
string (c:cs) = do
  char c
  string cs
  return (c:cs)

-- -- Explanation: we have a list with pairs [(0, '0'), (1, '1')] etc.
-- -- We map it with function, that should create parser for each that char, and return int of that char.
-- -- When some function is mapped to a parser, it will execute on parsed value AFTER parsing. So,
-- -- when whe map 'const v' to parser, it means, that parser will HAVE TO parse some character, and then
-- -- instead of that character it will return corresponding digit no matter what it's input is, as long
-- -- as it was parsed without error. oneOf just folds all parsers with alternative <|>, which means that
-- -- from list of parsers we get parser of 'lists' of chars
-- digit :: Parser Integer
-- digit = oneOf $ (\(v, c) -> const v <$> char c) <$> zip [0..] ['0'..'9']

-- unsignedInteger :: Parser Integer
-- unsignedInteger = foldl (\acc item -> item + acc * 10) 0 <$> some digit

-- negativeInteger :: Parser Integer
-- negativeInteger = (*) (-1) <$> (char '-' *> unsignedInteger)

-- integer :: Parser Integer
-- integer = unsignedInteger <|> negativeInteger

between :: Parser a -> Parser b -> Parser c -> Parser a
between p left right = left *> p <* right

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

-- jsonNumber :: Parser Json
-- jsonNumber = JsonNumber <$> integer

-- element :: Parser Json
-- element = ws *> jsonValue <* ws

-- jsonArray :: Parser Json
-- jsonArray = JsonArray <$>
--   (between (const [] <$> ws) (char '[') (char ']')
--    <|> between elements (char '[') (char ']'))
--   where
--     elements = manySepBy element (char ',')

-- jsonObject :: Parser Json
-- jsonObject = JsonObject <$>
--   (between (const [] <$> ws) (char '{') (char '}')
--    <|> between members (char '{') (char '}'))
--   where
--     member = do
--       ws
--       jsonKey <- jsonString
--       ws
--       char ':'
--       value <- element
--       case jsonKey of
--         JsonString key -> return (key, value)
--         _ -> empty
--     members = manySepBy member (char ',')

-- jsonValue :: Parser Json
-- jsonValue =
--   jsonObject <|>
--   jsonArray  <|>
--   jsonNumber <|>
--   jsonString <|>
--   jsonTrue   <|>
--   jsonFalse  <|>
--   jsonNull

-- -- Usage: parse jsonParser *input*
-- jsonParser :: Parser Json
-- jsonParser = ws *> jsonValue <* ws

