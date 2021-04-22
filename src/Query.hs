module Query where
import QueryParser
import JsonParser
import Data.List.Split
import Data.List

--------------------- FILTERING -----------------------

filterJson :: [Query] -> Json -> [Json]
filterJson qs json = filterWithPipes (semanticCheckForFilter qs) json
  where filterWithPipes qs json =
          let pipedQueries = splitOn [Pipe] qs
          in foldl' (\json qs -> concatMap (filterSeparated qs) json) [json] pipedQueries

        filterSeparated qs json =
            let separateQs = splitOn [Comma] qs
            in concatMap (flip filterWithOriginal json) separateQs

semanticCheckForFilter :: [Query] -> [Query]
semanticCheckForFilter qs =
  -- TODO: figure out how to add rules that will forbid given elements together
  if length qs > 0
  then qs
  else error "Can't !!!"

filterWithOriginal :: [Query] -> Json -> [Json] -- TODO: better name
filterWithOriginal queries original = filter' queries original
  where filter' :: [Query] -> Json -> [Json]
        filter' [] json     = [json]
        filter' (q:qs) json =
          case q of -- todo: improve quality of this case expression
            (Array (Index i)) -> filter' qs $ head $ filterOneQuery q json
            (Array _) -> forArray
            -- semantic chech should ensure that there will always be something after compare
            (Compare c) -> if filterCompare q (head qs) json
                           then [original]
                           else []
            otherwise -> filter' qs $ head $ filterOneQuery q json
          where forArray = filterOneQuery q json ++ filterForArray qs json

filterForArray :: [Query] -> Json -> [Json] -- TODO: come up with better name
filterForArray [] _    = []
filterForArray qs json = filterJson qs json

filterOneQuery :: Query -> Json -> [Json]
filterOneQuery Dot json = [filterDot json]

filterOneQuery (Field name) json = [getField name json]

filterOneQuery (Array EmptyArray) (JsonArray xs) = xs
filterOneQuery (Array (Index i)) (JsonArray xs) = [xs !! i]
filterOneQuery (Array (IndexRange range@(l, r))) (JsonArray xs) =
  if l < 0 || r < 0 || r < l || l > length xs || r > length xs
  then error $ "Can't get slice of array " ++ show xs ++ " with given indices: " ++ show range
  else slice l r xs

filterOneQuery Comma json = [json]

filterOneQuery wtf json = error $ "Can't execute query " ++ show wtf ++ " " ++ show json -- todo: implement query generator?
-- TODO: add booleans
filterCompare :: Query -> Query -> Json -> Bool
filterCompare (Compare (QueryParser.EQ)) (QueryNumber x) (JsonNumber y) = x == y
filterCompare (Compare (QueryParser.EQ)) (QueryString x) (JsonString y) = x == y

filterCompare (Compare (QueryParser.NEQ)) (QueryNumber x) (JsonNumber y) = x /= y
filterCompare (Compare (QueryParser.NEQ)) (QueryString x) (JsonString y) = x /= y

filterCompare (Compare (QueryParser.GT)) (QueryNumber x) (JsonNumber y) = x < y
filterCompare (Compare (QueryParser.GE)) (QueryNumber x) (JsonNumber y) = x <= y

filterCompare (Compare (QueryParser.LT)) (QueryNumber x) (JsonNumber y) = x > y
filterCompare (Compare (QueryParser.LE)) (QueryNumber x) (JsonNumber y) = x >= y

filterCompare _ x y = error $ "Can't compare " ++ show y ++ " with " ++ show x

filterDot :: Json -> Json
filterDot json@(JsonArray _) = json -- todo: dot works only if json is array or object
filterDot json@(JsonObject _) = json
filterDot json = error $ "Can't get '.' of " ++ show json

getField :: String -> Json -> Json
getField name json =
  case json of
    JsonObject l -> case lookup name l of
                      Just v -> v
                      Nothing -> error $ "Can't get field " ++ name ++ " from json " ++ show json
    otherwise -> error $ "Can't get field " ++ name ++ ": json is not an object " ++ show json -- todo: improve errors

slice :: Int -> Int -> [a] -> [a]
slice l r = take (r - l) . drop l

--------------------- DUPLICATES -----------------------

-- TODO: implement for array and object
removeDuplicates :: Json -> Json
removeDuplicates json = undefined

  --------------------- SEARCHING -----------------------
searchJson :: [Query] -> Int -> Json -> [Json]
searchJson = undefined
