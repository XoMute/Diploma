module Main where

-- import Parser (Parser, parse, jsonParser) -- TODO: uncomment
import Parser
import Generator (generate, prettyPrint)

main :: IO ()
main = do
  jsonStr <- readFile "./test.json"
  let result = parse jsonParser (inputFrom jsonStr)
  case result of
    Right (json, _) -> print json
    Left error -> print error
 -- let generated = prettyPrint 0 json
--  putStrLn generated

-- test :: IO ()
-- test = do
--   let jsonStr = "{\"name1\": true, \"name2\": [1, 2, 3], \"name3\": {\"name4\": [null, \"Some timestamp\", {\"name5\": 0}]}}"
--   let result = parse jsonValue jsonStr
--   let json =
--         case result of
--           Just(json, _) -> json
--           _ -> JsonString "Error"
--   let generated = generate json
--   print json
--   putStrLn generated

-- testP :: IO ()
-- testP = do
--   let jsonStr = "{\"name1\": true, \"name2\": [1, 2, 3], \"name3\": {\"name4\": [null, \"Some timestamp\", {\"name5\": \"Value\"}]}}"
--   let result = parse jsonValue jsonStr
--   let json =
--         case result of
--           Just(json, _) -> json
--           _ -> JsonString "Error"
--   let pretty = prettyPrint 0 json
--   print json
--   putStrLn pretty

