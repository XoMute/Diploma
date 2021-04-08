module Query where
import QueryParser
import JsonParser

execute :: [Query] -> Json -> [Json]
execute [] json = [json]
execute (q:qs) json =
  case q of -- todo: improve quality of this case expression
    Comma -> forArray
    (Array (Index i)) -> execute qs $ head $ executeOneQuery q json
    (Array _) -> forArray
    otherwise -> execute qs $ head $ executeOneQuery q json
  where forArray = executeOneQuery q json ++ executeForArray qs json

executeForArray :: [Query] -> Json -> [Json] -- TODO: come up with better name
executeForArray [] _ = []
executeForArray qs json = execute qs json

executeOneQuery :: Query -> Json -> [Json]
executeOneQuery Dot json = [executeDot json]
executeOneQuery (Field name) json =  [getField name json]
executeOneQuery (Array EmptyArray) (JsonArray l) = l
executeOneQuery (Array (Index i)) (JsonArray l) = [l !! i]

executeOneQuery wtf json = error $ "Can't execute query " ++ show wtf ++ " " ++ show json-- todo: implement query generator

executeDot :: Json -> Json
executeDot json@(JsonArray _) = json -- todo: dot works only if json is array or object
executeDot json@(JsonObject _) = json
executeDot json = error $ "Can't get '.' of " ++ show json

getField :: String -> Json -> Json
getField name json =
  case json of
    JsonObject l -> case lookup name l of
                      Just v -> v
                      Nothing -> error $ "Can't get field " ++ name ++ " from json"
    otherwise -> error $ "Can't get field " ++ name ++ ": json is not an object " ++ show json -- todo: improve errors
