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
  | Minimize           -- -m
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

flags =
  [Option  ['f'] ["filter"]   (ReqArg Filter "EXPR")
    "Filter json by given expression."
   ,Option ['d'] ["dupl"]     (NoArg Duplicates)
    "Remove all duplicates from output after filtering."
   ,Option ['s'] ["search"]   (ReqArg Search "EXPR")
    "Find object with given field and value."
   ,Option ['p'] ["parent"]   (ReqArg (Parent . read) "NUM")
    "Works along with '-s' flag. Get parent of found object at given level, where level 0 is the object itself."
   ,Option ['m'] ["minimize"] (NoArg Minimize)
    "Minimize json output."
   ,Option ['h'] ["help"] (NoArg Help)
     "Display this help and exit."
  ]

readInt :: Maybe String -> Maybe Int
readInt m = m >>= readMaybe

-- returns all parsed flags and unparsed arguments
parseArgs :: [String] -> IO ([Flag], [String])
parseArgs argv = case getOpt Permute flags argv of

  (args, fs, []) -> do
    let files = if null fs then ["-"] else fs
    if Help `elem` args
      then do hPutStrLn stderr (usageInfo helpMessage flags)
              exitWith ExitSuccess
      else return (nub (concatMap set args), files)

  (_, _, errs)   -> do
    hPutStrLn stderr (concat errs ++ usageInfo helpMessage flags)
    exitWith (ExitFailure 1)

  where helpMessage = "Usage: *some_name* [OPTIONS] [FILES]"

        -- set Blanks = [Blanks, Number]
        -- set Dollar = [Dollar, Invisible]
        -- set Tabs   = [Tabs, Invisible]
        set f      = [f] -- todo: remove it completely?

  -- numberLine = printf "%6d  %s"
-- numberAll s = zipWith numberLine [(1 :: Integer)..] s
-- numberSome s = reverse . snd $ foldl' draw (1, []) s
--   where
--     draw (n, acc) s
--       | all isSpace s = (n, s : acc)
--       | otherwise     = (n + 1, numberLine n s : acc)

-- cat :: [Flag] -> String -> IO ()
-- cat [] f = CommandLine.withFile f id
-- cat args f = CommandLine.withFile f (newline . number . visible args)
--   where
--     number s = if Blanks `elem` args then numberSome s else ifset Number numberAll s
--     newline s = ifset Dollar (map (++ "$")) s
--     visible args s = foldl' (flip render) s args
--     ifset a f = if a `elem` args then f else id

-- main :: IO ()
-- main = do
--   (args, files) <- getArgs >>= parseArgs
--   when (Unbuffered `elem` args) $ hSetBuffering stdout NoBuffering
--   mapM_ (cat args) files

