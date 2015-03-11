-- Huniq, a haskell implementation of GNU uniq.
module Main where
import System.Environment
import System.Console.Terminfo.Base
import Data.List

main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs
  let options = processArgs args defaultUniq
--  runTermOutput term (termText ("Options: "++(show options)++"\n"))

  runTermOutput term (termText (showHelp options))
  runTermOutput term (termText (showVersion options))
--  input <- [oh, this!]
--  if (input == lastInput) then "" -- [and this!!!]
--                          else input
--  output <- showOutput options
--  runTermOutput term (termText (output))
  return ()

showOutput :: UniqOptions -> IO String
showOutput opts | not ((displayHelp opts) || (displayVersion opts)) = return "" -- <do-stuff-Here>
                | otherwise = return ""


showHelp :: UniqOptions -> String
showHelp opts | (displayHelp opts) = concat (intersperse "\n" helpText)
              | otherwise = ""

showVersion :: UniqOptions -> String
showVersion opts | (displayVersion opts) = concat (intersperse "\n" versionText)
                 | otherwise = ""

processArgs :: [String] -> UniqOptions -> UniqOptions
processArgs [] opts = opts
processArgs (x:xs) opts = case x of
  "--help" -> processArgs xs opts{displayHelp = True}
  "--version" -> processArgs xs opts{displayVersion = True}
  _ -> processArgs xs opts

stripQuotes :: String -> String
stripQuotes ('"':xs) = if last xs == '"' then init xs else ('"':xs)
stripQuotes xs = xs

defaultUniq :: UniqOptions
defaultUniq = UniqOptions False False False False DupeNone 0 GroupSeperate False 0 True False (-1)

data UniqOptions = UniqOptions
  { displayHelp :: Bool
  , displayVersion :: Bool
  , prefixCount :: Bool
  , onlyDuplicates :: Bool
  , allDuplicates :: DuplicateMethod
  , skipFirstNFields :: Integer
  , groupOutput :: GroupingMethod
  , ignoreCase :: Bool
  , skipFirstNChars :: Integer
  , onlyUnique :: Bool
  , suppressNewline :: Bool
  , checkNChars :: Integer } deriving (Show, Eq)

data DuplicateMethod = DupeNone | DupePrepend | DupeSeperate deriving (Show, Eq)
data GroupingMethod = GroupSeperate | GroupPrepend | GroupAppend | GroupBoth deriving (Show, Eq)

helpText :: [String]
helpText = [ "Usage: uniq [OPTION]... [INPUT [OUTPUT]]"
           , "Filter adjacent matching lines from INPUT (or standard input),"
           , "writing to OUTPUT (or standard output)."
           , "With no options, matching lines are merged to the first occurrence."
           , "Mandatory arguments to long options are mandatory for short options too."
           , "  -c, --count           prefix lines by the number of occurrences"
           , "  -d, --repeated        only print duplicate lines, one for each group"
           , "  -D, --all-repeated[=METHOD]  print all duplicate lines"
           , "                          groups can be delimited with an empty line"
           , "                          METHOD={none(default),prepend,separate}"
           , "  -f, --skip-fields=N   avoid comparing the first N fields"
           , "      --group[=METHOD]  show all items, separating groups with an empty line"
           , "                          METHOD={separate(default),prepend,append,both}"
           , "  -i, --ignore-case     ignore differences in case when comparing"
           , "  -s, --skip-chars=N    avoid comparing the first N characters"
           , "  -u, --unique          only print unique lines"
           , "  -z, --zero-terminated     line delimiter is NUL, not newline"
           , "  -w, --check-chars=N compare no more than N characters in lines"
           , "      --help display this help and exit"
           , "      --version output version information and exit"
           , "A field is a run of blanks (usually spaces and/or TABs), then non-blank"
           , "characters.  Fields are skipped before chars."
           , "Note: 'uniq' does not detect repeated lines unless they are adjacent."
           , "You may want to sort the input first, or use 'sort -u' without 'uniq'."
           , "Also, comparisons honor the rules specified by 'LC_COLLATE'."
           , "GNU coreutils online help: <http://www.gnu.org/software/coreutils/>"
           , "For complete documentation, run: info coreutils 'uniq invocation'\n"
           ]

versionText :: [String]
versionText = [ "Huniq (Haskell implementation of GNU uniq) 1.0"
              , "derrived from: uniq (GNU coreutils) 8.23"
              , "Copyright (C) 2014 Free Software Foundation, Inc."
              , "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
              , "This is free software: you are free to change and redistribute it."
              , "There is NO WARRANTY, to the extent permitted by law."
              , "Written by Richard M. Stallman and David MacKenzie."
              , "Ported by PuZZleDucK.\n"
              ]

