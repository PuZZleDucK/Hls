-- Hcat, a haskell implementation of GNU cat.
module Main where
import System.Environment
import System.Console.Terminfo.Base
import Data.List
import System.IO

main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs
  let options = processArgs args defaultCat
--  runTermOutput term (termText ("Options: "++(show options)++"\n"))

  runTermOutput term (termText (showHelp options))
  runTermOutput term (termText (showVersion options))
  output <- showOutput options
  runTermOutput term (termText (output))
  return ()

showOutput :: CatOptions -> IO String
showOutput opts | not ((displayHelp opts) || (displayVersion opts)) = do
  handles <- sequence (map ((flip openFile) ReadMode) ((targetFiles opts)))
  contents <- sequence (map hGetContents (handles))
  return (concat contents)
                | otherwise = return ""


showHelp :: CatOptions -> String
showHelp opts | (displayHelp opts) = concat (intersperse "\n" helpText)
              | otherwise = ""

showVersion :: CatOptions -> String
showVersion opts | (displayVersion opts) = concat (intersperse "\n" versionText)
                 | otherwise = ""

processArgs :: [String] -> CatOptions -> CatOptions
processArgs [] opts = opts
processArgs (x:xs) opts = case x of
  "--help" -> processArgs xs opts{displayHelp = True}
  "--version" -> processArgs xs opts{displayVersion = True}
  z -> processArgs xs opts{targetFiles = (targetFiles opts++[z])}

stripQuotes :: String -> String
stripQuotes ('"':xs) = if last xs == '"' then init xs else ('"':xs)
stripQuotes xs = xs

defaultCat :: CatOptions
defaultCat = CatOptions False False [] False False False False False False

data CatOptions = CatOptions
  { displayHelp :: Bool
  , displayVersion :: Bool
  , targetFiles :: [String]
  , displayNonBlankCount :: Bool
  , displayLineNumbers :: Bool
  , displayLineTerminators :: Bool
  , suppressMultipleBlank :: Bool
  , displayVisibleTab :: Bool
  , displayVisibleNonPrinting :: Bool
  } deriving (Show, Eq)


helpText :: [String]
helpText = [ "Usage: /home/bminerds/x/coreutils/src/cat [OPTION]... [FILE]..."
           , "Concatenate FILE(s), or standard input, to standard output."
           , "  -A, --show-all           equivalent to -vET"
           , "  -b, --number-nonblank    number nonempty output lines, overrides -n"
           , "  -e                       equivalent to -vE"
           , "  -E, --show-ends          display $ at end of each line"
           , "  -n, --number             number all output lines"
           , "  -s, --squeeze-blank      suppress repeated empty output lines"
           , "  -t                       equivalent to -vT"
           , "  -T, --show-tabs          display TAB characters as ^I"
           , "  -u                       (ignored)"
           , "  -v, --show-nonprinting   use ^ and M- notation, except for LFD and TAB"
           , "      --help     display this help and exit"
           , "      --version  output version information and exit"
           , "With no FILE, or when FILE is -, read standard input."
           , "Examples:"
           , "  /home/bminerds/x/coreutils/src/cat f - g  Output f's contents, then standard input, then g's contents."
           , "  /home/bminerds/x/coreutils/src/cat        Copy standard input to standard output."
           , "GNU coreutils online help: <http://www.gnu.org/software/coreutils/>"
           , "Full documentation at: <http://www.gnu.org/software/coreutils/cat>"
           , "or available locally via: info '(coreutils) cat invocation'\n"
           ]

versionText :: [String]
versionText = [ "Hcat (Haskell implementation of GNU cat) 1.0"
              , "derrived from: cat (GNU coreutils) 8.23.126-99f76"
              , "Copyright (C) 2015 Free Software Foundation, Inc."
              , "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
              , "This is free software: you are free to change and redistribute it."
              , "There is NO WARRANTY, to the extent permitted by law."
              , "Written by Torbjörn Granlund and Richard M. Stallman."
              , "Ported by PuZZleDucK.\n"
              ]

