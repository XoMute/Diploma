module Main where

-- import Parser (Parser, parse, jsonParser) -- TODO: uncomment
import Parser
import JsonParser
import Generator (generate, prettyPrint)
import QueryParser

main :: IO ()
main = do
  jsonStr <- readFile "./test.json"
  let result = parse jsonParser (inputFrom jsonStr)
  let json = resultToJson result
  let generated = prettyPrint 0 json
  putStrLn generated

testError :: IO ()
testError = do
  let jsonStr = "{\"name1\": 2.3e5, \"name2\":}"
  let result = parse jsonParser (inputFrom jsonStr)
  -- why the fuck is it so hard to understand how to show correct errors
  print result

test :: IO ()
test = do
  let jsonStr = "{\"name1\": true, \"name2\": [1, 2, 3], \"name3\": {\"name4\": [null, \"Some timestamp\", {\"name5\": 0}]}}"
  let result = parse jsonParser (inputFrom jsonStr)
  let json = resultToJson result
  let generated = generate json
  putStrLn generated

testP :: IO ()
testP = do
  let jsonStr = "{\"name1\": true, \"name2\": [1, 2, 3], \"name3\": {\"name4\": [null, \"Some timestamp\", {\"name5\": \"Value\"}]}}"
  let result = parse jsonParser (inputFrom jsonStr)
  let json = resultToJson result
  let pretty = prettyPrint 0 json
  putStrLn pretty
