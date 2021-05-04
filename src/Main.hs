module Main where

import Parser
import JsonParser
import Generator (generate, prettify)
import QueryParser
import Filtration
import CommandLine

import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad
import Control.Applicative
import System.Environment
import System.Exit

main :: IO ()
main = do
  (args, file) <- getArgs >>= parseArgs
  js <- run (reverse args) file
  let res   = if Duplicates `elem` args
              then nub js
              else js
  let jsons = if Minimize `elem` args
              then map generate res
              else map (prettify $ getIndent args) res
  mapM_ putStrLn jsons
  where getIndent args =
          let (Indentation i) = fromMaybe (Indentation 4) $ find isIndent args
          in if i < 0
             then 0
             else i

run :: [Flag] -> String -> IO [Json]
run [] file = (:[]) <$> parseFile file jsonParser

run args file = do
  when (hasFilter && hasSearch) $
    die "Please, specify either '-f' or '-s' options."
  when (hasParent && not hasSearch) $
    die "Please, specify '-s' option along with '-p'."
  json <- parseFile file jsonParser
  filtered <- if hasFilter then
    do
      query <- resultToQuery $ parse queryParser (inputFrom $ getFilterQuery)
      either die pure
        $ filterJson query json
    else pure [json]
  searched <- if hasSearch then
    do
      query <- resultToQuery $ parse queryParser (inputFrom $ getSearchQuery)
      case searchJson query getParentLevel json of
        Left err -> die err
        Right res ->
          if null res then
            die "No object found."
          else if One `elem` args then
                 pure [head res]
          else pure res
    else pure filtered
  return searched
  where hasFilter = optionIsPresent (Filter "") args -- todo: replace all has* with checking of get*Query result (with removed `fromJust')
        hasSearch = optionIsPresent (Search "") args
        hasParent = optionIsPresent (Parent 0) args

        getFilterQuery = let (Filter query) = fromJust $ find isFilter args
                         in L.pack query
        getSearchQuery = let (Search query) = fromJust $ find isSearch args
                         in L.pack query
        getParentLevel = let (Parent level) = fromMaybe (Parent 0) $ find isParent args
                         in level

parseFile :: String -> Parser Json -> IO Json
parseFile file parser = do
  content <- open file
  let result = parse parser $ inputFrom content
  resultToJson result
  where
    open f = if f == "-" then L.pack <$> getContents else L.readFile f

resultToJson :: Either ParseError (Json, Input) -> IO Json
resultToJson result =
  either
    (\err -> die $ show err)
    (\(json, _) -> pure json)
    result

resultToQuery :: Either ParseError ([Query], Input) -> IO [Query]
resultToQuery result =
  either
    (\err -> die $ show err)
    (\(query, _) -> pure query)
    result

