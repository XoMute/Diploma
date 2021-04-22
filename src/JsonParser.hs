module JsonParser where
import Parser
import Control.Applicative

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
jsonArray = JsonArray <$> (between (const [] <$> ws) (char '[') (char ']')
                      <|>  between elements (char '[') (char ']'))
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
jsonValue = oneOf [
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

resultToJson :: Either ParseError (Json, Input) -> Json
resultToJson result =
  either
    (\err -> JsonString $ show err)
    (\(json, _) -> json)
    result
