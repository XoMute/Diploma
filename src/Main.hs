module Main where

import Parser
import JsonParser
import Generator (generate, prettyPrint)
import QueryParser
import Query
import CommandLine
import System.Environment
-- import System.IO

main :: IO ()
main = do
  (args, files) <- getArgs >>= parseArgs
  -- TODO: unparsed argument should be just ONE file
  res <- run args (head files)
  let pretty = map (prettyPrint 0) res
  mapM_ putStrLn pretty

run :: [Flag] -> String -> IO ([Json])
run [] file = (:[]) <$> parseFile file jsonParser
run ((Filter query):args) file = do
  json <- parseFile file jsonParser
  let queryResult = parse queryParser (inputFrom query)
  let query = resultToQuery queryResult
  let res = filterJson query json
  return res
run ((Duplicates query):args) file = do -- TODO
  json <- parseFile file jsonParser
  let queryResult = parse queryParser (inputFrom query)
  let query = resultToQuery queryResult
  let res = filterJson query json
  return res

run args _ = error $ "Not implemented yet: " ++ show args

parseFile :: String -> Parser Json -> IO (Json)
parseFile file parser = do
  content <- open file
  let result = parse parser $ inputFrom content
  return $ resultToJson result
  where
    open f = if f == "-" then getContents else readFile f

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
  let res = filterJson query json
  let pretty = map (prettyPrint 0) res
  mapM_ putStrLn pretty

testQuery1 :: String -> IO ()
testQuery1 queryStr = do
  let jsonStr = "[{\"foo\": {\"bar\": {\"buz\": 1}}}, {\"foo\": {\"bar\": {\"buz\": 2}}}, {\"foo\": {\"bar\": {\"buz\": 3}}}]"
  let result = parse jsonParser (inputFrom jsonStr)
  let json = resultToJson result
  let queryResult = parse queryParser (inputFrom queryStr)
  let query = resultToQuery queryResult
  let res = filterJson query json
  let pretty = map (prettyPrint 0) res
  mapM_ putStrLn pretty
