module Main where

-- import Parser (Parser, parse, jsonParser) -- TODO: uncomment
import Parser
import JsonParser
import Generator (generate, prettyPrint)
import QueryParser
import Query
import CommandLine
import System.Environment
-- import System.IO
-- import System.Exit

main :: IO ()
main = do
  (args, files) <- getArgs >>= parseArgs
  -- TODO: unparsed argument should be just ONE file
  run args (head files)
  -- mapM_ (cat args) files
  -- jsonStr <- readFile "./test.json"
  -- let result = parse jsonParser (inputFrom jsonStr)
  -- let json = resultToJson result
  -- let generated = prettyPrint 0 json
  -- putStrLn generated

run :: [Flag] -> String -> IO ()
run [] file = parseFile file jsonParser
--run args file = 

parseFile :: String -> Parser Json -> IO ()
parseFile file parser = do
  content <- open file
  let result = parse parser $ inputFrom content
  putStrLn $ prettyPrint 0 $ resultToJson result
  where
    open f = if f == "-" then getContents else readFile f
--  putStr . parse parser  =<< open file

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

testQuery :: String -> IO ()
testQuery queryStr = do
  let jsonStr = "{\"name1\": true, \"name2\": [1, 2, 3], \"name3\": {\"name4\": [null, \"Some timestamp\", {\"name5\": \"Value\"}]}}"
  let result = parse jsonParser (inputFrom jsonStr)
  let json = resultToJson result
  let queryResult = parse queryParser (inputFrom queryStr)
  let query = resultToQuery queryResult
  print query
  let res = execute query json
  let pretty = map (prettyPrint 0) res
  mapM_ putStrLn pretty

