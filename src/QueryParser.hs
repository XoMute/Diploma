-- Reference: https://stedolan.github.io/jq/manual
-- This module contains functions to parse given query.
-- For example:
--   query "." returns toplevel object as itself
--   query ".foo" returns value of given field of main object, if it is present, and error (exception?) otherwise
--   query ".[<index>]" can index arrays
--   query ".[<index1>:<index2>]" will return array slice (exclusive for index2)
--   query ".[]" returns all elements of toplevel array as separate lines
--   query ".foo[]" returns all elements of foo array as separate lines
--   query ".foo, .bar" will separate two different outputs for foo and bar values
--   query ".[] | .foo" will retrieve all foo fields from array elements.
--   query ".[] | .foo == 1" will retrieve all elements from array in which foo fields equal to 1.
--   query "[.]" returns array, element of which will be current object
--   query "{field1: .foo}" will generate json "{\"field1\": *some-value-foo*}"
--   Also: Conditionals on which to choose given fields? -- No, should do that as separate parameter

module QueryParser where
import Parser
import Control.Applicative
import Data.Char (isDigit, isLetter)

number :: Parser Int
number = read <$> digits
  where digits = some $ parseWhen isDigit

data Comparison
  = LT
  | LE
  | GT
  | GE
  | EQ
  | NEQ
  deriving (Show, Eq)

data ArrayIndex
  = EmptyArray
  | Index Int
  | IndexRange (Int, Int) -- (inclusive, exclusive)
  deriving (Show, Eq)

data Query
  = Dot
  | Array ArrayIndex
  | Pipe
  | Field String
  | Comma
  | Compare Comparison
  | QueryNumber Number
  | QueryString String
  | QueryBool Bool
  deriving (Show, Eq)

dot :: Parser Query
dot = Dot <$ char '.'

pipe :: Parser Query
pipe = Pipe <$ (ws *> char '|' <* ws)

comma :: Parser Query
comma = Comma <$ (ws *> char ',' <* ws)

queryTrue :: Parser Query
queryTrue = QueryBool True <$ string "true"

queryFalse :: Parser Query
queryFalse = QueryBool False <$ string "false"

index :: Parser ArrayIndex
index = Index <$> number

indexRange :: Parser ArrayIndex
indexRange = do
  left <- number
  char ':'
  right <- number
  return $ IndexRange (left, right)

array :: Parser Query
array = Array <$> (
  try (between (const EmptyArray <$> ws) (char '[') (char ']')) <|>
  try (between index (char '[') (char ']')) <|>
  between (failing indexRange) (char '[') (char ']'))

fieldNameChar :: Parser Char
fieldNameChar = parseWhen (\c -> isDigit c || isLetter c || c == '_')

-- TODO: https://jsonapi.org/format/#document-member-names
field :: Parser Query
field = Field <$> do
  c1   <- parseWhen (\c -> isLetter c || c == '_')
  rest <- many $ fieldNameChar
  return (c1:rest)

queryNumber :: Parser Query
queryNumber = QueryNumber <$> (ws *> double)

queryString :: Parser Query
queryString = QueryString <$> between (many character) (char '"') (char '"')

comparison :: Parser Query
comparison = Compare <$> (ws *> tryOneOf [le, ge, lt, gt, eq, neq, err] <* ws)
  where lt  = const QueryParser.LT  <$> string "<"
        le  = const QueryParser.LE  <$> string "<="
        gt  = const QueryParser.GT  <$> string ">"
        ge  = const QueryParser.GE  <$> string ">="
        eq  = const QueryParser.EQ  <$> string "=="
        neq = const QueryParser.NEQ <$> string "!="
        err = errorParser "comparison"

-- since queries will be much more shorter than JSONs - backtracking is not bad here
query :: Parser [Query]
query = (ws *> qs <* ws) >>= eof
  where qs =
          many $
          tryOneOf [
              queryTrue,
              queryFalse,
              array,
              field,
              queryNumber,
              queryString,
              comparison,
              dot,
              comma,
              pipe
              ]

queryParser :: Parser [Query]
queryParser = query

