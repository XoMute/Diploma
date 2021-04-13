module Query where
import QueryParser
import JsonParser
import Data.List.Split
import Data.List

execute :: [Query] -> Json -> [Json]
execute qs json =
  let pipedQueries = splitOn [Pipe] qs
  in foldl' (\json qs -> concatMap (executeWithOriginal qs) json) [json] pipedQueries

executeWithOriginal :: [Query] -> Json -> [Json] -- TODO: better name
executeWithOriginal queries original = execute' queries original
  where execute' :: [Query] -> Json -> [Json]
        execute' [] json     = [json]
        execute' (q:qs) json =
          case q of -- todo: improve quality of this case expression
            Comma -> json:execute' qs original
            (Array (Index i)) -> execute' qs $ head $ executeOneQuery q json
            (Array _) -> forArray
            otherwise -> execute' qs $ head $ executeOneQuery q json
          where forArray = executeOneQuery q json ++ executeForArray qs json

executeForArray :: [Query] -> Json -> [Json] -- TODO: come up with better name
executeForArray [] _    = []
executeForArray qs json = execute qs json

executeOneQuery :: Query -> Json -> [Json]
executeOneQuery Dot json = [executeDot json]

executeOneQuery (Field name) json = [getField name json]

executeOneQuery (Array EmptyArray) (JsonArray xs) = xs
executeOneQuery (Array (Index i)) (JsonArray xs) = [xs !! i]
executeOneQuery (Array (IndexRange range@(l, r))) (JsonArray xs) =
  if l < 0 || r < 0 || r < l || l > length xs || r > length xs
  then error $ "Can't get slice of array " ++ show xs ++ " with given indices: " ++ show range
  else slice l r xs

executeOneQuery Comma json = [json]

executeOneQuery wtf json = error $ "Can't execute query " ++ show wtf ++ " " ++ show json -- todo: implement query generator?

executeDot :: Json -> Json
executeDot json@(JsonArray _) = json -- todo: dot works only if json is array or object
executeDot json@(JsonObject _) = json
executeDot json = error $ "Can't get '.' of " ++ show json

getField :: String -> Json -> Json
getField name json =
  case json of
    JsonObject l -> case lookup name l of
                      Just v -> v
                      Nothing -> error $ "Can't get field " ++ name ++ " from json " ++ show json
    otherwise -> error $ "Can't get field " ++ name ++ ": json is not an object " ++ show json -- todo: improve errors

slice :: Int -> Int -> [a] -> [a]
slice l r = take (r - l) . drop l
