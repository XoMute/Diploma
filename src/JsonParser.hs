module JsonParser where
import Parser
import Control.Applicative
import Data.Char
import Data.List
import Data.ByteString.Lazy.Char8 (unpack)

data Json
  = JsonNull
  | JsonBool Bool
  | JsonString String
  | JsonNumber Double
  | JsonArray [Json]
  | JsonObject [(String, Json)]
  deriving (Show, Ord)

instance Eq Json where
  JsonNull    == JsonNull        = True
  JsonBool b1 == JsonBool b2     = b1 == b2
  JsonString s1 == JsonString s2 = s1 == s2
  JsonNumber n1 == JsonNumber n2 = n1 == n2
  JsonArray j1 == JsonArray j2   = sort j1 == sort j2
  JsonObject o1 == JsonObject o2 = sortOn fst o1 == sortOn fst o2
  j1 == j2                       = False

jsonNull :: Parser Json
jsonNull = JsonNull <$ string "null"

jsonTrue :: Parser Json
jsonTrue = JsonBool True <$ string "true"

jsonFalse :: Parser Json
jsonFalse = JsonBool False <$ string "false"

jsonString :: Parser Json
jsonString = JsonString <$> between (many character) (char '"') (char '"')

jsonKey :: Parser Json
jsonKey = JsonString <$> between field (char '"') (char '"') -- TODO: rewrite
  where field = do
          c1   <- parseWhen (\c -> isLetter c || c == '_')
          rest <- many $ fieldNameChar
          return (c1:rest)

fieldNameChar :: Parser Char
fieldNameChar = parseWhen (\c -> isDigit c || isLetter c || c == '_')

jsonNumber :: Parser Json
jsonNumber = JsonNumber <$> double

element :: Parser Json
element = ws *> jsonValue <* ws

jsonArray :: Parser Json
jsonArray = JsonArray <$> (try (between (const [] <$> ws) (char '[') (char ']'))
                      <|>  between elements (char '[') (char ']'))
  where
    elements = manySepBy (failing element) (char ',')

jsonObject :: Parser Json
jsonObject = JsonObject <$>
  (try (between (const [] <$> ws) (char '{') (char '}'))
   <|> between members (char '{') (char '}'))
  where
    member = do
      ws
      key <- jsonKey
      ws
      char ':'
      value <- element
      case key of
        JsonString k -> return (k, value)
        _ -> empty
    members = manySepBy (failing member) (char ',')

jsonValue :: Parser Json
jsonValue = oneOf [
  jsonObject,
  jsonArray,
  jsonNumber,
  jsonString,
  jsonTrue,
  jsonFalse,
  jsonNull,
  errorParser "JSON value"]

-- Usage: parse jsonParser *input*
jsonParser :: Parser Json
jsonParser = element

