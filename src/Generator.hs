module Generator
  (
    prettyPrint,
    generate,
    showQuery
  )
where
import JsonParser (Json(JsonNull,
                        JsonBool,
                        JsonString,
                        JsonNumber,
                        JsonArray,
                        JsonObject))
import QueryParser
import Data.List

type Indent = Int

spaces :: Int -> String
spaces n = take n $ repeat ' '

between :: String -> String -> String -> String
between l r str = l ++ str ++ r

trim :: String -> String
trim [] = []
trim (x:xs)
  | x == ' ' = trim xs
  | otherwise = x:xs

----------- PRETTY PRINT -----------

prettyPrint :: Indent -> Indent -> Json -> String
prettyPrint i indent (JsonNull) = spaces i ++ "null"

prettyPrint i indent (JsonBool True)  = spaces i ++ "true"
prettyPrint i indent (JsonBool False) = spaces i ++ "false"

prettyPrint i indent (JsonString str) = spaces i ++ between "\"" "\"" str

prettyPrint i indent (JsonNumber num) = spaces i ++ show num

prettyPrint i indent (JsonArray [])       = "[]"
prettyPrint i indent (JsonArray elements) =
  between (spaces i ++ "[\n") ("\n" ++ spaces i ++ "]") $
  intercalate ",\n" (map (prettyPrint (i + indent) indent) elements)

prettyPrint i indent (JsonObject []) = "{}"
prettyPrint i indent (JsonObject elements) =
  between (spaces i ++ "{\n") ("\n" ++ spaces i ++ "}") $
  intercalate ",\n" $
  map (\(name, value) -> between (spaces (i + indent) ++ "\"") "\"" name ++
                         ": " ++
                         trim (prettyPrint (i + indent) indent value)) elements

----------- GENERATION -----------

generate :: Json -> String
generate (JsonNull) = "null"

generate (JsonBool True) = "true"
generate (JsonBool False) = "false"

generate (JsonString str) = between "\"" "\"" str

generate (JsonNumber num) = show num

generate (JsonArray []) = "[]"
generate (JsonArray elements) =
  between "[" "]"
  (intercalate "," (map (generate) elements))

generate (JsonObject []) = "{}"
generate (JsonObject elements) =
  between "{" "}" $
  intercalate "," $
  map (\(name, value) -> between "\"" "\"" name ++ ":" ++ generate value) elements

------------- QUERY ---------------

showQuery :: Query -> String
showQuery Dot = "'.'"
showQuery (Array EmptyArray) = "'[]'"
showQuery (Array (Index i)) = "'[" ++ show i ++ "]'"
showQuery (Array (IndexRange (l, r))) = "'[" ++ show l ++ ":" ++ show r ++ "]'"
showQuery Pipe = "'|'"
showQuery Comma = "','"
showQuery (Field f) = "Field name '" ++ f ++ "'"
showQuery q = show q

-- data Comparison
--   = LT
--   | LE
--   | GT
--   | GE
--   | EQ
--   | NEQ
--   deriving (Show, Eq, Data)

-- data ArrayIndex
--   = EmptyArray
--   | Index Int
--   | IndexRange (Int, Int) -- (inclusive, exclusive)
--   deriving (Show, Eq, Data)

-- data Query
--   | Pipe
--   | Field String
--   | QueryNumber Double
--   | Comma
--   | Compare Comparison
--   | QueryString String
--   deriving (Show, Eq, Typeable, Data)
