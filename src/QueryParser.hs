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
import Data.Char (isDigit)

number :: Parser Int
number = read <$> digits
  where digits = some $ parseWhen "Digit" isDigit

data Query
  = Dot
  | Object [(String, Query)] -- ??
  | Array Query
  | Pipe
  | Field String
  | Index Int
  | IndexRange (Int, Int) -- (inclusive, exclusive)
  | Comma
  deriving (Show, Eq)

dot :: Parser Query
dot = Dot <$ char '.'

pipe :: Parser Query
pipe = Pipe <$ char '|'

comma :: Parser Query
comma = Comma <$ char ','

index :: Parser Query
index = Index <$> number

indexRange :: Parser Query
indexRange = do
  left <- number
  char ':'
  right <- number
  return $ IndexRange (left, right)

array :: Parser Query
array = Array <$>
  between index (char '[') (char ']') <|>
  between indexRange (char '[') (char ']')

field :: Parser Query
field = Field <$> many character

object :: Parser Query
object = undefined

--   query ".foo" (or .["foo"]) returns value of given field of main object, if it is present, and error (exception?) otherwise
--   query ".[<index>]" can index arrays
--   query ".[<index1>:<index2>]" will return array slice (exclusive for index2)
--   query ".[]" returns all elements of toplevel array as separate lines
--   query ".foo[]" returns all elements of foo array as separate lines
--   query ".foo, .bar" will separate two different outputs for foo and bar values
--   query ".[] | .foo" will retrieve all foo fields from array elements.
--   query "[.]" returns array, element of which will be current object -- PARSE THIS IN THE LAST MOMENT
--   query "{field1: .foo}" will generate json "{\"field1\": *some-value-foo*}"
query :: Parser [Query] -- [Query]
query = -- CONST IS WRONG!!! TRY :[]
  (:[]) <$> object <|>
  const [] <$> array <|>
  fieldQuery <|>
  const [] <$>   dot <|>
  const [] <$>   commaQuery <|>
  const [] <$>   pipeQuery <|>
  empty

fieldQuery :: Parser [Query]
fieldQuery = do
  dot
  fld <- field
  qr <- query
  return $ fld : qr

commaQuery :: Parser Query
commaQuery = undefined

pipeQuery :: Parser Query
pipeQuery = undefined

-- queryCommaExp :: Parser Query
-- queryCommaExp = QueryCommaExp <$> manySepBy "Query comma expression" expression comma

-- queryPipeExp :: Parser Query
-- queryPipeExp = QueryPipeExp <$> manySepBy "Query pipe expression" expression pipe

-- queryExp :: Parser Query
-- queryExp = queryCommaExp <|> queryPipeExp

queryParser :: Parser Query
queryParser = undefined
