-- Hprintenv, a haskell implementation of GNU printenv.
module Main where

import Data.List
import System.Console.Terminfo.Base
import System.Environment

--import Data.Text
main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs
  let options = processArgs args defaultPrintenv
  runTermOutput term (termText (showHelp options))
  runTermOutput term (termText (showVersion options))
  output <- showOutput options
  runTermOutput term (termText (output))
  return ()

--  runTermOutput term (termText ("Options: "++(show options)++"\n"))
showOutput :: PrintenvOptions -> IO String
showOutput opts
  | not ((displayHelp opts) || (displayVersion opts)) = do
    env <- formatEnvironment opts
    return (concat env)
  | otherwise = return ""
  where
    _tgts = targets opts

--  putStrLn (show tgts)
--  putStrLn (show env)
--needs more serious departure: don't print key-name when looking up individual variables.
formatEnvironment :: PrintenvOptions -> IO [String]
formatEnvironment opts = do
  e <- getEnvironment
  let r =
        if (targets opts) == []
          then e
          else filter (\x -> (elem (fst x) (targets opts))) e
  return
    ((map
        (\(k, v) ->
           (stripQuotes (show k)) ++ "=" ++ (stripQuotes (show v)) ++ sep)
        r))
  where
    sep =
      if suppressNewline opts == True
        then ""
        else "\n"

showHelp :: PrintenvOptions -> String
showHelp opts
  | (displayHelp opts) = concat (intersperse "\n" helpText)
  | otherwise = ""

showVersion :: PrintenvOptions -> String
showVersion opts
  | (displayVersion opts) = concat (intersperse "\n" versionText)
  | otherwise = ""

processArgs :: [String] -> PrintenvOptions -> PrintenvOptions
processArgs [] opts = opts
processArgs (x:xs) opts =
  case x of
    "--help" -> processArgs xs opts {displayHelp = True}
    "--version" -> processArgs xs opts {displayVersion = True}
    "--null" -> processArgs xs opts {suppressNewline = True}
    z -> processArgs xs opts {targets = (targets opts) ++ [z]}

stripQuotes :: String -> String
stripQuotes ('"':xs) =
  if last xs == '"'
    then init xs
    else ('"' : xs)
stripQuotes xs = xs

defaultPrintenv :: PrintenvOptions
defaultPrintenv = PrintenvOptions False False False []

data PrintenvOptions = PrintenvOptions
  { displayHelp :: Bool
  , displayVersion :: Bool
  , suppressNewline :: Bool
  , targets :: [String]
  } deriving (Show, Eq)

helpText :: [String]
helpText =
  [ "Usage: /home/bminerds/x/coreutils/src/printenv [OPTION]... [VARIABLE]..."
  , "Print the values of the specified environment VARIABLE(s)."
  , "If no VARIABLE is specified, print name and value pairs for them all."
  , "  -0, --null     end each output line with NUL, not newline"
  , "      --help     display this help and exit"
  , "      --version  output version information and exit"
  , "NOTE: your shell may have its own version of printenv, which usually supersedes"
  , "the version described here.  Please refer to your shell's documentation"
  , "for details about the options it supports."
  , "GNU coreutils online help: <http://www.gnu.org/software/coreutils/>"
  , "Full documentation at: <http://www.gnu.org/software/coreutils/printenv>"
  , "or available locally via: info '(coreutils) printenv invocation'\n"
  ]

versionText :: [String]
versionText =
  [ "Hprintenv (Haskell implementation of GNU printenv) 1.0"
  , "derrived from: printenv (GNU coreutils) 8.23.126-99f76"
  , "Copyright (C) 2015 Free Software Foundation, Inc."
  , "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
  , "This is free software: you are free to change and redistribute it."
  , "There is NO WARRANTY, to the extent permitted by law."
  , "Written by David MacKenzie and Richard Mlynarik."
  , "Ported by PuZZleDucK.\n"
  ]
