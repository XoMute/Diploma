{-# LANGUAGE DeriveDataTypeable #-}
module CommandLine where

import Control.Monad
import Data.Char
import Data.List
import Data.Data
import Data.Function
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf
import Text.Read

data Flag
  = Filter String      -- -f
  | Duplicates         -- -d
  | Search String      -- -s
  | Parent Int         -- -p
  | One                -- -o
  | Minimize           -- -m
  | Indentation Int    -- -i
  | Help               -- --help
  deriving (Show, Eq, Ord, Typeable, Data)

optionIsPresent :: Flag -> [Flag] -> Bool
optionIsPresent f = isJust . find ((==) $ toConstr f) . map toConstr

isFilter :: Flag -> Bool
isFilter (Filter _) = True
isFilter f = False

isSearch :: Flag -> Bool
isSearch (Search _) = True
isSearch f = False

isParent :: Flag -> Bool
isParent (Parent _) = True
isParent f = False

isIndent :: Flag -> Bool
isIndent (Indentation _) = True
isIndent f = False

flags =
  [Option  ['f'] ["filter"]   (ReqArg Filter "EXPR")
    "Filter json by given expression."
   ,Option ['d'] ["dupl"]     (NoArg Duplicates)
    "Remove all duplicates from output after filtering or searching."
   ,Option ['s'] ["search"]   (ReqArg Search "EXPR")
    "Find object with given field and value."
   ,Option ['p'] ["parent"]   (ReqArg (Parent . read) "NUM")
    "Works along with '-s' flag. Get parent of found object at given level, where level 0 is the object itself."
   ,Option ['o'] ["one"]      (NoArg One)
    "Return only first result of filter or search."
   ,Option ['m'] ["minimize"] (NoArg Minimize)
    "Minimize json output."
   ,Option ['i'] ["indent"]   (ReqArg (Indentation . read) "NUM")
    "Number of spaces in indentation."
   ,Option ['h'] ["help"]     (NoArg Help)
     "Display this help and exit."
  ]

readInt :: Maybe String -> Maybe Int
readInt m = m >>= readMaybe

-- returns all parsed flags and unparsed arguments
parseArgs :: [String] -> IO ([Flag], String)
parseArgs argv = case getOpt Permute flags argv of

  (args, fs, []) -> do
    let files = if null fs then ["-"] else fs
    if Help `elem` args
      then do hPutStrLn stderr (usageInfo helpMessage flags)
              exitWith ExitSuccess
      else if length files > 1 then
             die "Only one file should be provided."
      else return (nub args, head files)

  (_, _, errs)   -> do
    hPutStrLn stderr (concat errs ++ usageInfo helpMessage flags)
    exitWith (ExitFailure 1)

  where helpMessage = "Usage: hson [OPTIONS] [FILES]"

