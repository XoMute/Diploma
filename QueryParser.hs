-- ASK: is it ok to copy expression types from jq
-- Reference: https://stedolan.github.io/jq/manual
-- This module contains functions to parse given query and to execute it on the json.
-- For example:
--   query "." returns toplevel object as itself
--   query ".foo" (or .["foo"]) returns value of given field of main object, if it is present, and error (exception?) otherwise
--   query ".[<index>]" can index arrays
--   query ".[<index1>:<index2>]" will return array slice (exclusive for index2)
--   query ".[]" returns all elements of toplevel array as separate lines
--   query ".foo[]" returns all elements of foo array as separate lines
--   query ".foo, .bar" will separate two different outputs for foo and bar values
--   query ".[] | .foo" will retrieve all foo fields from array elements.
--   query "[.]" returns array, element of which will be current object
--   query "{field1: .foo}" will generate json "{\"field1\": *some-value-foo*}"
--   Also: Conditionals on which to choose given fields?

module QueryParser where
import Parser

data Query
  = Dot
  | Object
  | Array
  | Pipe
  | Field
  | Index
  | Comma
  deriving (Show, Eq)

parseDot :: Parser Query
parseDot = Dot <$ char '.'

parsePipe :: Parser Query
parsePipe = Pipe <$ char '|'

parseComma :: Parser Query
parseComma = Comma <$ char ','

parseQuery :: Parser Query
parseQuery = undefined
