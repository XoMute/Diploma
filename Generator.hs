module Generator
  (
    prettyPrint,
    generate
  )
where
import Parser (Json(JsonNull,
                    JsonBool,
                    JsonString,
                    JsonNumber,
                    JsonArray,
                    JsonObject))
import Data.List

type Indent = Int

spaces :: Int -> String
spaces n = take n $ repeat ' '

between :: String -> String -> String -> String
between l r str = l ++ str ++ r

----------- PRETTY PRINT -----------

prettyPrint :: Indent -> Json -> String
prettyPrint i (JsonNull) = spaces i ++ "null"

prettyPrint i (JsonBool True) = spaces i ++ "true"
prettyPrint i (JsonBool False) = spaces i ++ "false"

prettyPrint i (JsonString str) = spaces i ++ str

prettyPrint i (JsonNumber num) = spaces i ++ show num

prettyPrint i (JsonArray []) = "[]"
prettyPrint i (JsonArray elements) =
  between (spaces i ++ "[\n") ("\n" ++ spaces i ++ "]")
  (intercalate ",\n" (map (prettyPrint (i + 2)) elements))

prettyPrint i (JsonObject []) = "{}"
prettyPrint i (JsonObject [(property, value)]) = undefined

prettyPrint i json = spaces i ++ (generate json)

----------- GENERATION -----------

generate :: Json -> String
generate (JsonNull) = "null"

generate (JsonBool True) = "true"
generate (JsonBool False) = "false"

generate (JsonString str) = str

generate (JsonNumber num) = show num

generate (JsonArray []) = "[]"
generate (JsonArray elements) =
  between "[" "]"
  (intercalate "," (map (generate) elements))

generate (JsonObject []) = "{}"
generate (JsonObject [(property, value)]) = undefined

