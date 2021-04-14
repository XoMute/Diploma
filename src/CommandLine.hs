module CommandLine where

import Control.Monad
import Data.Char
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

data Flag
  = Filter String  -- -f
  | Remove         -- -r
  | Split          -- -s
  | Merge          -- -m
  | Duplicates String -- -d
  | Validate       -- -v
  | Help           -- --help
  deriving (Show, Eq, Ord)

flags =
  [Option  ['f']  ["filter"]  (ReqArg Filter "EXPR")
    "Filter json by given expression."
   ,Option ['d'] ["dupl"]     (ReqArg Duplicates "EXPR")
    "Filter json by given expression and remove all duplicates."
   ,Option ['v'] ["validate"] (NoArg Validate)
    "Validate json by given expression."
    -- ,Option ['e'] [] (NoArg Dollar)
  --   "Implies the -v option and also prints a dollar sign at the end of each line."
  -- ,Option ['s'] [] (NoArg Squeeze)
  --   "Squeeze multiple adjacent empty lines, causing the output to be single-spaced."
  -- ,Option ['t'] [] (NoArg Tabs)
  --   "Implies the -v option and also prints tab characters as `^I'."
  -- ,Option ['u'] [] (NoArg Unbuffered)
  --   "The output is guaranteed to be unbuffered (see setbuf(3))."
  -- ,Option ['v'] [] (NoArg Invisible)
  --   "Displays non-printing characters so they are visible."
  -- ,Option ['n'] [] (NoArg Number)
  --   "Number the output lines, starting at 1."
  ,Option ['h'] ["help"] (NoArg Help)
    "Display this help and exit."
  ]

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

