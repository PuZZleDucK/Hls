-- Hsort, a haskell implementation of GNU sort.
module Main where
import System.Environment
import System.Console.Terminfo.Base
import Data.List

main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs
  let options = processArgs args defaultSort
  runTermOutput term (termText ("Options: "++(show options)++"\n"))

  runTermOutput term (termText (showHelp options))
  runTermOutput term (termText (showVersion options))
  output <- showOutput options
  runTermOutput term (termText (output))
  return ()

showOutput :: SortOptions -> IO String
showOutput opts | not ((displayHelp opts) || (displayVersion opts)) = return "" -- <do-stuff-Here>
                | otherwise = return ""


showHelp :: SortOptions -> String
showHelp opts | (displayHelp opts) = concat (intersperse "\n" helpText)
              | otherwise = ""

showVersion :: SortOptions -> String
showVersion opts | (displayVersion opts) = concat (intersperse "\n" versionText)
                 | otherwise = ""

processArgs :: [String] -> SortOptions -> SortOptions
processArgs [] opts = opts
processArgs (x:xs) opts = case x of
  "--help" -> processArgs xs opts{displayHelp = True}
  "--version" -> processArgs xs opts{displayVersion = True}
  _ -> processArgs xs opts

stripQuotes :: String -> String
stripQuotes ('"':xs) = if last xs == '"' then init xs else ('"':xs)
stripQuotes xs = xs

defaultSort :: SortOptions
defaultSort = SortOptions False False "" "" [] "" 0 False False False False [] False "" False 0 "" [] 1 False False

data SortOptions = SortOptions
  { displayHelp :: Bool
  , displayVersion :: Bool
  , sortText :: String
  , sortTargets :: String
  , ordering :: [SortOrderingOptions]
  , randomSource :: String
  , batchSize :: Integer
  , checkSorting :: Bool
  , compressTemporary :: Bool
  , debugMode :: Bool
  , quietMode :: Bool
  , sortKey :: [KeyDef]
  , onlyMerge :: Bool
  , outputFile :: String
  , stabilizeSort :: Bool
  , bufferSize :: Integer
  , fieldSeperator :: String
  , tempDirectories :: [String]
  , concurrencyLevel :: Integer
  , uniqueEntriesOnly :: Bool
  , nullTerminated :: Bool
  } deriving (Show, Eq)

data SortOrderingOptions = IgnoreLeadingBlanks | DictionarySorting | IgnoreCase | GeneralNumericSorting | IgnoreUnprintable | MonthSorting | HumanNumericSorting | NumericSorting | RandomSorting | ReverseOrdering | VersionSorting deriving (Show, Eq)

data KeyDef = KeyDef{ fieldNumber :: Integer, characterOffset :: Integer, sortOptions :: [SortOrderingOptions]} deriving (Show, Eq)
--KEYDEF is F[.C][OPTS][,F[.C][OPTS]] for start and stop position, where F is a field number and C a character position in the field; both are origin 1, and the stop position defaults to the line's end

--OPTS is one or more single-letter ordering options [bdfgiMhnRrV], which override global ordering options for that key


--SIZE: b 1, K 1024 (default), and so on for M, G, T, P, E, Z, Y.

helpText :: [String]
helpText = [ "Usage: /home/bminerds/x/coreutils/src/sort [OPTION]... [FILE]..."
           , "  or:  /home/bminerds/x/coreutils/src/sort [OPTION]... --files0-from=F"
           , "Write sorted concatenation of all FILE(s) to standard output."
           , "Mandatory arguments to long options are mandatory for short options too."
           , "Ordering options:"
           , "  -b, --ignore-leading-blanks  ignore leading blanks"
           , "  -d, --dictionary-order      consider only blanks and alphanumeric characters"
           , "  -f, --ignore-case           fold lower case to upper case characters"
           , "  -g, --general-numeric-sort  compare according to general numerical value"
           , "  -i, --ignore-nonprinting    consider only printable characters"
           , "  -M, --month-sort            compare (unknown) < 'JAN' < ... < 'DEC'"
           , "  -h, --human-numeric-sort    compare human readable numbers (e.g., 2K 1G)"
           , "  -n, --numeric-sort          compare according to string numerical value"
           , "  -R, --random-sort           sort by random hash of keys"
           , "      --random-source=FILE    get random bytes from FILE"
           , "  -r, --reverse               reverse the result of comparisons"
           , "      --sort=WORD             sort according to WORD: general-numeric -g, human-numeric -h, month -M, numeric -n, random -R, version -V"
           , "  -V, --version-sort          natural sort of (version) numbers within text"
           , "Other options:"
           , "      --batch-size=NMERGE   merge at most NMERGE inputs at once; for more use temp files"
           , "  -c, --check, --check=diagnose-first  check for sorted input; do not sort"
           , "  -C, --check=quiet, --check=silent  like -c, but do not report first bad line"
           , "      --compress-program=PROG  compress temporaries with PROG; decompress them with PROG -d"
           , "      --debug               annotate the part of the line used to sort, and warn about questionable usage to stderr"
           , "      --files0-from=F       read input from the files specified by NUL-terminated names in file F; If F is - then read names from standard input"
           , "  -k, --key=KEYDEF          sort via a key; KEYDEF gives location and type"
           , "  -m, --merge               merge already sorted files; do not sort"
           , "  -o, --output=FILE         write result to FILE instead of standard output"
           , "  -s, --stable              stabilize sort by disabling last-resort comparison"
           , "  -S, --buffer-size=SIZE    use SIZE for main memory buffer"
           , "  -t, --field-separator=SEP  use SEP instead of non-blank to blank transition"
           , "  -T, --temporary-directory=DIR  use DIR for temporaries, not $TMPDIR or /tmp; multiple options specify multiple directories"
           , "      --parallel=N          change the number of sorts run concurrently to N"
           , "  -u, --unique              with -c, check for strict ordering; without -c, output only the first of an equal run"
           , "  -z, --zero-terminated     line delimiter is NUL, not newline"
           , "      --help     display this help and exit"
           , "      --version  output version information and exit"
           , "KEYDEF is F[.C][OPTS][,F[.C][OPTS]] for start and stop position, where F is a field number and C a character position in the field; both are origin 1, and the stop position defaults to the line's end."
           , "If neither -t nor -b is in effect, characters in a field are counted from the beginning of the preceding whitespace."
           , "OPTS is one or more single-letter ordering options [bdfgiMhnRrV], which override global ordering options for that key."
           , "If no key is given, use the entire line as the key."
           , "SIZE may be followed by the following multiplicative suffixes: % 1% of memory, b 1, K 1024 (default), and so on for M, G, T, P, E, Z, Y."
           , "With no FILE, or when FILE is -, read standard input."
           , "*** WARNING ***"
           , "The locale specified by the environment affects sort order."
           , "Set LC_ALL=C to get the traditional sort order that uses native byte values."
           , "GNU coreutils online help: <http://www.gnu.org/software/coreutils/>"
           , "Full documentation at: <http://www.gnu.org/software/coreutils/sort>or available locally via: info '(coreutils) sort invocation'\n"
           ]

versionText :: [String]
versionText = [ "H<app-name> (Haskell implementation of GNU <app-name>) 1.0"
              , "derrived from: sort (GNU coreutils) 8.23.126-99f76"
              , "Copyright (C) 2015 Free Software Foundation, Inc."
              , "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
              , "This is free software: you are free to change and redistribute it."
              , "There is NO WARRANTY, to the extent permitted by law."
              , "Written by Mike Haertel and Paul Eggert."
              , "Ported by PuZZleDucK.\n"
              ]

