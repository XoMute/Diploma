module Generator
  (
    prettyPrint,
    generate
  )
where
import JsonParser (Json(JsonNull,
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

trim :: String -> String
trim [] = []
trim (x:xs)
  | x == ' ' = trim xs
  | otherwise = x:xs

----------- PRETTY PRINT -----------

-- TODO: try to do that in this way
-- class (Expr e) => Pretty e where
--     pretty :: e -> String

-- instance Pretty Const where
--     pretty (Const x) = show x
-- instance (Pretty a, Pretty b) => Pretty (Add a b) where
--     pretty (Add x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"
-- instance (Pretty a, Pretty b) => Pretty (Mul a b) where
--     pretty (Mul x y) = pretty x ++ " * " ++ pretty y

prettyPrint :: Indent -> Json -> String
prettyPrint i (JsonNull) = spaces i ++ "null"

prettyPrint i (JsonBool True) = spaces i ++ "true"
prettyPrint i (JsonBool False) = spaces i ++ "false"

prettyPrint i (JsonString str) = spaces i ++ between "\"" "\"" str

prettyPrint i (JsonNumber num) = spaces i ++ show num

prettyPrint i (JsonArray []) = "[]"
prettyPrint i (JsonArray elements) =
  between (spaces i ++ "[\n") ("\n" ++ spaces i ++ "]") $
  intercalate ",\n" (map (prettyPrint (i + 2)) elements)

prettyPrint i (JsonObject []) = "{}"
prettyPrint i (JsonObject elements) =
  between (spaces i ++ "{\n") ("\n" ++ spaces i ++ "}") $
  intercalate ",\n" $
  map (\(name, value) -> between (spaces (i + 2) ++ "\"") "\"" name ++ ": " ++ trim (prettyPrint (i + 2) value)) elements
-- TODO: figure out something better than 'trim'

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

