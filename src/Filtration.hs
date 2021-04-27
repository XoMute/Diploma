module Filtration where
import QueryParser
import JsonParser
import Generator

import Data.List.Split
import Data.List
import Control.Monad
import System.Exit

--------------------- FILTERING -----------------------

-- TODO: replace IO with Either
filterJson :: [Query] -> Json -> IO [Json]
filterJson qs json = do
  let pipedQueries = splitOn [Pipe] qs
  foldM (\json qs -> concat <$> mapM (filterSeparated qs) json) [json] pipedQueries

filterSeparated :: [Query] -> Json -> IO [Json]
filterSeparated qs json = do
  separateQs <- mapM semanticCheckForFilter $ splitOn [Comma] qs
  concat <$> mapM (flip filterWithOriginal json) separateQs

filterWithOriginal :: [Query] -> Json -> IO [Json] -- TODO: better name
filterWithOriginal queries original = filter' queries original
  where filter' :: [Query] -> Json -> IO [Json]
        filter' [] json     = pure [json]
        filter' (q:qs) json =
          case q of -- todo: improve quality of this case expression
            (Array (Index i)) -> filterOneQuery q json >>= filter' qs . head
            (Array _) -> forArray

            -- semantic check ensures that there will always be something after `Compare'
            (Compare c) -> do
              res <- filterCompare q (head qs) json
              if res then
                pure [original]
                else pure []
            otherwise -> filterOneQuery q json >>= filter' qs . head
          where forArray = do
                  js1 <- filterOneQuery q json
                  js2 <- filterForArray qs json
                  pure $ js1 ++ js2

filterForArray :: [Query] -> Json -> IO [Json] -- TODO: come up with better name
filterForArray [] _    = pure []
filterForArray qs json = filterJson qs json

filterOneQuery :: Query -> Json -> IO [Json]
filterOneQuery Dot json = filterDot json >>= pure . (:[])

filterOneQuery (Field name) json = getField name json >>= pure . (:[])

filterOneQuery (Array EmptyArray) (JsonArray xs) = pure xs
filterOneQuery (Array (Index i)) (JsonArray xs) = pure [xs !! i]
filterOneQuery (Array (IndexRange range@(l, r))) (JsonArray xs) =
  if l < 0 || r < 0 || r < l || l > length xs || r > length xs
  then die $ "Can't get slice of array " ++ show xs ++ " with given indices: " ++ show range
  else pure $ slice l r xs

filterOneQuery Comma json = pure [json]

filterOneQuery q json = die $ "Can't execute query " ++ show q ++ " " ++ show json

filterCompare :: Query -> Query -> Json -> IO Bool
filterCompare (Compare (QueryParser.EQ)) (QueryNumber x) (JsonNumber y) = pure $ x == y
filterCompare (Compare (QueryParser.EQ)) (QueryString x) (JsonString y) = pure $ x == y
filterCompare (Compare (QueryParser.EQ)) (QueryBool x) (JsonBool y)     = pure $ x == y

filterCompare (Compare (QueryParser.NEQ)) (QueryNumber x) (JsonNumber y) = pure $ x /= y
filterCompare (Compare (QueryParser.NEQ)) (QueryString x) (JsonString y) = pure $ x /= y
filterCompare (Compare (QueryParser.NEQ)) (QueryBool x) (JsonBool y)     = pure $ x /= y

filterCompare (Compare (QueryParser.GT)) (QueryNumber x) (JsonNumber y) = pure $ x < y
filterCompare (Compare (QueryParser.GE)) (QueryNumber x) (JsonNumber y) = pure $ x <= y

filterCompare (Compare (QueryParser.LT)) (QueryNumber x) (JsonNumber y) = pure $ x > y
filterCompare (Compare (QueryParser.LE)) (QueryNumber x) (JsonNumber y) = pure $ x >= y

filterCompare _ x y = die $ "Can't compare " ++ show y ++ " with " ++ show x

filterDot :: Json -> IO Json
filterDot json@(JsonArray _)  = pure json
filterDot json@(JsonObject _) = pure json
filterDot (JsonBool b)        = die $ dotErrMsg $ show b
filterDot (JsonNull)          = die $ dotErrMsg "null"
filterDot (JsonNumber _)      = die $ dotErrMsg "number"
filterDot (JsonString _)      = die $ dotErrMsg "string"

dotErrMsg :: String -> String
dotErrMsg = (++) "'.' can be used only after array or object, not "

getField :: String -> Json -> IO Json
getField name json =
  case json of
    JsonObject l -> case lookup name l of
                      Just v -> pure v
                      Nothing -> die $ "Can't get field \"" ++ name ++ "\" from given JSON value"
    otherwise -> die $ "Can't get field \"" ++ name ++ "\": given JSON value is not an object "

slice :: Int -> Int -> [a] -> [a]
slice l r = take (r - l) . drop l

  --------------------- SEARCHING -----------------------
-- NOTE: search won't go :down if it found correct object
searchJson :: [Query] -> Int -> Json -> IO [(Json, Int)]
searchJson [] _ _  = die "Query can't be empty."
searchJson q@(Field f:[]) p json@(JsonArray js) =
  concat <$> mapM (searchWithParent q p json) js

searchJson q@(Field f:[]) p json@(JsonObject js) =
  case lookup f js of
    Just _  -> pure [(json, 0)]
    Nothing -> searchDown q p js >>= return . map (resultToParent p json)

searchJson (Field _:[]) _ _ = pure []

searchJson qs@(Field f:Compare _:_:[]) p json@(JsonArray js) =
  concat <$> mapM (searchWithParent qs p json) js

searchJson qs@(Field f:cmp@(Compare _):q:[]) p json@(JsonObject js) =
  case lookup f js of
    Just v  -> do
      res <- filterCompare cmp q v
      if res then
        pure [(json, 0)]
        else searchHelper qs p js
    Nothing -> searchHelper qs p js
  where searchHelper qs p js = searchDown qs p js >>= return . map (resultToParent p json)

searchJson (Field _:Compare _:_:[]) p json = pure []

searchJson qs _ _= die "Search query is wrong. Possible search queries:\n - \"FIELD_NAME\"\n - \"FIELD_NAME *COMPARE* LITERAL_VALUE\""

searchDown :: [Query] -> Int -> [(a, Json)] -> IO [(Json, Int)]
searchDown qs p js =
  concat <$>
  (mapM (searchJson qs p) $
   filter containerP $ snd $ unzip js)

searchWithParent :: [Query] -> Int -> Json -> Json -> IO [(Json, Int)]
searchWithParent q p json j = searchJson q p j >>= pure . map (resultToParent p json)

resultToParent :: Int -> Json -> (Json, Int) -> (Json, Int)
resultToParent p json (res, level)
  | level < p = (json, level + 1)
  | otherwise = (res, level)

containerP :: Json -> Bool
containerP (JsonArray  _) = True
containerP (JsonObject _) = True
containerP json           = False

----------------------- SEMANTIC CHECK ------------------
semanticCheckForFilter :: [Query] -> IO [Query]
semanticCheckForFilter [] = die "Query can't be empty."
semanticCheckForFilter qs = do
  case head qs of
    Dot -> semCheck qs
    otherwise -> die "All queries should start with '.'"

semCheck :: [Query] -> IO [Query]
semCheck (q@Dot: qs@(Array _:_))   = semCheck qs >>= pure . (q:)
semCheck (q@Dot: qs@(Field _:_))   = semCheck qs >>= pure . (q:)
semCheck (q@Dot: qs@(Compare _:_)) = semCheck qs >>= pure . (q:)
semCheck q@(Dot:[]) = pure q

semCheck (q@(Array (Index _)):  qs@(Dot:_))        = semCheck qs >>= pure . (q:)
semCheck (q@(Array (Index _)):  qs@(Compare _:_))  = semCheck qs >>= pure . (q:)
semCheck q@(Array _:[]) = pure q

semCheck (q@(Field _):qs@(Dot:_))       = semCheck qs >>= pure . (q:)
semCheck (q@(Field _):qs@(Array _:_))   = semCheck qs >>= pure . (q:)
semCheck (q@(Field _):qs@(Compare _:_)) = semCheck qs >>= pure . (q:)
semCheck q@(Field _:[]) = pure q

semCheck (q@(Compare _):qs@(QueryNumber _:_)) = semCheck qs >>= pure . (q:)
semCheck (q@(Compare _):qs@(QueryString _:_)) = semCheck qs >>= pure . (q:)
semCheck (q@(Compare _):qs@(QueryBool _:_))   = semCheck qs >>= pure . (q:)

semCheck (q1:q2:_) = die $ showQuery q2 ++ " can't be used after " ++ showQuery q1
semCheck (q@(QueryNumber _):[]) = pure [q]
semCheck (q@(QueryString _):[]) = pure [q]
semCheck (q@(QueryBool _):[]) = pure [q]
semCheck (q:[]) = die $ showQuery q ++ " can't be used alone."

