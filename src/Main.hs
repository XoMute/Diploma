module Main where

import Parser
import JsonParser
import Generator (generate, prettyPrint)
import QueryParser
import Query
import CommandLine
import System.Environment
import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative
-- import System.IO

main :: IO ()
main = do
  (args, files) <- getArgs >>= parseArgs
  -- TODO: unparsed argument should be just ONE file
  js <- run (reverse args) (head files)
  let res   = if Duplicates `elem` args
              then map removeDuplicates js
              else js
  let jsons = if Minimize `elem` args
              then map generate res
              else map (prettyPrint 0) res
  mapM_ putStrLn jsons

run :: [Flag] -> String -> IO ([Json])
run [] file = (:[]) <$> parseFile file jsonParser

run args file = do
  when (hasFilter && hasSearch) $
    error "Please, specify either '-f' or '-s' options."
  when (hasParent && not hasSearch) $
    error "Please, specify '-s' option along with '-p'."
  json <- parseFile file jsonParser
  filtered <- if hasFilter then
    do
      let query = resultToQuery $ parse queryParser (inputFrom $ getFilterQuery)
      let res = filterJson query json
      return res
    else pure $ filterJson [] json
  searched <- if hasSearch then
    do
      let query = resultToQuery $ parse queryParser (inputFrom $ getSearchQuery)
      let res = searchJson query getParentLevel json
      return res
    else pure filtered
  return searched
  where hasFilter = optionIsPresent (Filter "") args -- todo: replace all has* with checking of get*Query result (with removed `fromJust')
        hasSearch = optionIsPresent (Search "") args
        hasParent = optionIsPresent (Parent 0) args

        getFilterQuery = let (Filter query) = fromJust $ find isFilter args
                         in query
        getSearchQuery = let (Search query) = fromJust $ find isSearch args
                         in query
        getParentLevel = let (Parent level) = fromMaybe (Parent 0) $ find isParent args
                         in level

parseFile :: String -> Parser Json -> IO (Json)
parseFile file parser = do
  content <- open file
  let result = parse parser $ inputFrom content
  return $ resultToJson result
  where
    open f = if f == "-" then getContents else readFile f

