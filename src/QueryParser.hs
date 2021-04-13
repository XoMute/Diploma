-- ASK: is it ok to copy expression types from jq
-- Reference: https://stedolan.github.io/jq/manual
-- This module contains functions to parse given query and to execute it on the json.
-- For example:
--   query "." returns toplevel object as itself
--   query ".foo" returns value of given field of main object, if it is present, and error (exception?) otherwise
--   query ".[<index>]" can index arrays
--   query ".[<index1>:<index2>]" will return array slice (exclusive for index2)
--   query ".[]" returns all elements of toplevel array as separate lines
--   query ".foo[]" returns all elements of foo array as separate lines
--   query ".foo, .bar" will separate two different outputs for foo and bar values
--   query ".[] | .foo" will retrieve all foo fields from array elements.
--   query "[.]" returns array, element of which will be current object
--   query "{field1: .foo}" will generate json "{\"field1\": *some-value-foo*}"
--   Also: Conditionals on which to choose given fields? -- No, should do that as separate parameter

module QueryParser where
import Parser
import Control.Applicative
import Data.Char (isDigit, isLetter)

number :: Parser Int
number = read <$> digits
  where digits = some $ parseWhen "Digit" isDigit

data Query
  = Dot
  | Object [(String, Query)] -- ??
  | Array Query -- don't use Query here
  | Pipe
  | Field String
  | Index Int
  | IndexRange (Int, Int) -- (inclusive, exclusive)
  | EmptyArray
  | Comma
  deriving (Show, Eq)

dot :: Parser Query
dot = Dot <$ char '.'

pipe :: Parser Query
pipe = Pipe <$ (ws1 *> char '|' <* ws1)

comma :: Parser Query
comma = Comma <$ (ws *> char ',' <* ws)

index :: Parser Query
index = Index <$> number

indexRange :: Parser Query
indexRange = do
  left <- number
  char ':' -- TODO: error if not found
  right <- number -- TODO: fail if nothing? Or even error
  return $ IndexRange (left, right)

array :: Parser Query
array = Array <$> (
  between (const EmptyArray <$> ws) (char '[') (char ']') <|>
  between index (char '[') (char ']') <|>
  between indexRange (char '[') (char ']'))

field :: Parser Query
field = Field <$> some fieldNameCharacter

fieldNameCharacter :: Parser Char
fieldNameCharacter = parseWhen "Name of field" (\c -> isDigit c || isLetter c)

object :: Parser Query
object = empty--undefined

--   query ".foo" returns value of given field of main object, if it is present, and error (exception?) otherwise -- DONE
--   query ".[<index>]" can index arrays -- DONE
--   query ".[<index1>:<index2>]" will return array slice (exclusive for index2) -- DONE
--   query ".[]" returns all elements of toplevel array as separate lines -- DONE
--   query ".foo[]" returns all elements of foo array as separate lines -- DONE
--   query ".foo, .bar" will separate two different outputs for foo and bar values -- DONE
--   query ".[] | .foo" will retrieve all foo fields from array elements.
--   query "[.]" returns array, element of which will be current object -- PARSE THIS IN THE LAST MOMENT
--   query "{field1: .foo}" will generate json "{\"field1\": *some-value-foo*}"

query :: Parser [Query]
query = many $ oneOf "query" [
  -- (:[]) <$> object <|>
  -- (:[]) <$> array <|>
  dot,
  comma,
  pipe,
  array,
  field--,
  ]

commaQuery :: Parser Query
commaQuery = empty--undefined

pipeQuery :: Parser Query
pipeQuery = empty--undefined

-- queryCommaExp :: Parser Query
-- queryCommaExp = QueryCommaExp <$> manySepBy "Query comma expression" expression comma

-- queryPipeExp :: Parser Query
-- queryPipeExp = QueryPipeExp <$> manySepBy "Query pipe expression" expression pipe

-- queryExp :: Parser Query
-- queryExp = queryCommaExp <|> queryPipeExp

queryParser :: Parser [Query]
queryParser = failIfNotFinished query -- todo: if empty array or input is left - parse error

resultToQuery :: Either ParseError ([Query], Input) -> [Query]
resultToQuery result =
  either
    (\err -> error $ "AAA " ++ show err)
    (\(query, _) -> query)
    result
