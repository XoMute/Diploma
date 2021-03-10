module Main where

-- import Parser (Parser, parse, jsonParser) -- TODO: uncomment
import Parser
import Generator

main :: IO ()
main = do
  jsonStr <- readFile "./test.json"
  let result = parse jsonParser jsonStr
  print result

test :: IO ()
test = do
  let jsonStr = "[1, 2, 3, true, \"String\"]"
  let result = parse jsonArray jsonStr
  let json =
        case result of
          Just(json, _) -> json
          _ -> JsonString "Error"
  let generated = generate json
  print json
  print generated

testP :: IO ()
testP = do
  let jsonStr = "[1, 2, 3, true, \"String\", [false, true, 1, 2]]"
  let result = parse jsonArray jsonStr
  let json =
        case result of
          Just(json, _) -> json
          _ -> JsonString "Error"
  let pretty = prettyPrint 0 json
  print json
  putStrLn pretty

