{-# LANGUAGE FlexibleInstances #-} -- todo: read about this
{-# LANGUAGE OverloadedStrings #-}

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
import Control.Monad
import Data.Char (isDigit)
import Data.Function
import Data.Maybe
import Data.Either
import qualified Data.ByteString.Lazy.Char8 as L

--               col  line
type Position = (Int, Int)
data Input = Input { pos :: Position, inputStr :: L.ByteString } deriving (Show, Eq) -- todo: do I need Eq?

inputFrom :: L.ByteString -> Input
inputFrom = Input (1, 1)

-- Get first character from input if it is present
nextChar :: Input -> Maybe (Char, Input)
nextChar (Input _ "") = Nothing
nextChar (Input (posX, posY) inp)
  | x == '\n' = Just (x, Input (1, posY + 1) xs)
  | otherwise = Just (x, Input (posX + 1, posY) xs)
  where x =  L.head inp
        xs = L.tail inp
--                                    expected  actual
data ParseError = ParseError Position String    String

instance Show ParseError where
  show (ParseError (column, line) expected actual) = "Line: " ++ show line ++ ", column: " ++
                                             show column ++ "\nUnexpected " ++ actual ++
                                             "\nExpecting " ++ expected

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

instance Monad Parser where
  return a = Parser $ \input -> pure (a, input)
  p >>= f = Parser $ \input ->
    case parse p input of
      Right (p', cs) -> parse (f p') cs
      Left error -> Left error

instance Alternative Parser where
-- empty parser always fails without consuming input
  empty = Parser $ \input -> Left $ ParseError (pos input) "" ""
-- It fails if left parser consumed input
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    let originalPos = pos input
        res1        = p1 input
    in
       case res1 of
         Left (ParseError position _ _) ->
           if position /= originalPos
           then res1
           else p2 input
         Right _ -> res1

-- check if input is left and return left if it is
failIfNotFinished :: Parser a -> String -> Parser a
failIfNotFinished p exp = Parser $ \inp ->
  let result = parse p inp in
    case result of
      Right (_, inp') ->
        if L.null (inputStr inp')
        then result
        else Left $ ParseError (pos inp') exp $ (L.unpack $ inputStr inp')
      otherwise -> result

-- returns parser that will not consume input in case of failing
try :: Parser a -> Parser a
try (Parser p) = Parser $ \input ->
    case p input of
      Left (ParseError _ exp act) -> Left (ParseError (pos input) exp act)
      res -> res

-- returns parser that will always consume input in case of failing
failing :: Parser a -> Parser a
failing (Parser p) = Parser $ \input ->
  case p input of
      Left (ParseError (c, l) exp act) -> Left (ParseError (c + 1, l) exp act)
      res -> res

-- returns parser with given error message
errorParser :: String -> Parser a
errorParser message = Parser $ \input -> Left (ParseError (pos input) message (L.unpack $ inputStr input))

oneOf :: [Parser a] -> Parser a
oneOf = foldl (<|>) empty

tryOneOf :: [Parser a] -> Parser a
tryOneOf = foldl ((<|>) `on` try) empty

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
          ParseError (pos input) (quote c) "end of input"
        Just (x, input') ->
          if c == x then Right (c, input')
          else Left $
               ParseError (pos input) (quote c) (quote x)

eof :: a -> Parser a
eof val = Parser $ \input ->
  if L.length (inputStr input) > 0
  then Left (ParseError (pos input) "end of file" (L.unpack $ inputStr input))
  else pure (val, input)

whitespace :: Parser Char
whitespace = oneOf [char '\n', char '\r', char '\t', char ' ']

ws :: Parser String -- L.Text
ws = many whitespace

ws1 :: Parser String -- L.Text
ws1 = some whitespace

character :: Parser Char
character =
  parseWhen
    (\c -> c /= '"' && c /= '\\') <|>
  escape

escape :: Parser Char
escape = tryOneOf [
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
string s = Parser $ \input ->
  case parse (traverse char s) input of
    Left (ParseError pos _ _) -> Left (ParseError pos ("\"" ++ s ++ "\"") (L.unpack $ inputStr input))
    res -> res

data Number = Number {
  sign     :: Integer,
  integral :: Integer,
  decimal  :: Maybe Double,
  expnt    :: Maybe Integer,
  num      :: Double
  }

instance Show Number where
  show (Number sign integral decimal expnt _) =
    showSign                ++
    (show integral)         ++
    (maybe "" show decimal) ++
    showExpnt
    where showSign  = if sign == 1 then "" else "-"
          showExpnt = (maybe "" (\val -> "e" ++ show val) expnt)

instance Eq Number where
  (==) = (==) `on` num

instance Ord Number where
  compare = (compare) `on` num

constructNumber :: Integer ->
                   Integer ->
                   Either Double Double ->
                   Either Integer Integer ->
                   Number
constructNumber sign integral decimal exponent =
  Number {
  sign = sign,
  integral = integral,
  decimal = either (const Nothing) Just decimal,
  expnt = either (const Nothing) Just exponent,
  num = fromIntegral sign *
        (fromIntegral integral + (fromRight 0.0 decimal))
        * (10 ^^ (fromRight 0 exponent))
  }

double :: Parser Number
double = let minus = (-1) <$ char '-'
             plus = 1 <$ char '+'
             digits = some $ parseWhen isDigit
             e = char 'e' <|> char 'E'
         in do
             sign <- minus <|> pure 1
             integral <- read <$> digits
             -- append "0." to digits and read result as double
             decimal <- ((Right . read) <$> (("0." ++) <$> (char '.' *> digits))) <|> pure (Left 0)
             -- multiply read number by 1 or -1 (depending on sign or it's absence)
             exponent <- (e *> (Right <$> (liftA2 (*)
                                           (plus <|> minus <|> pure 1)
                                           (read <$> digits))))
                         <|> pure (Left 0)
             return (constructNumber sign integral decimal exponent)

-- (liftA (*) ((fromRight 1) <$> (plus <|> minus <|> pure (Right 1))) (read <$> digits))

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
      ParseError (pos input) "Next character" "end of input"
    Just (x, input') ->
      if pred x then Right (x, input')
      else Left $
           ParseError (pos input) "" (quote x)

