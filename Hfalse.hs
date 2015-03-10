-- Hfalse, a haskell implementation of GNU false.
module Main where
import System.Environment
import System.Console.Terminfo.Base
import Data.List

main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs
  let options = processArgs args defaultFalse

  runTermOutput term (termText (showHelp options))
  runTermOutput term (termText (showVersion options))
  -- error ""
  return () -- how do I return an error... without printing...

showHelp :: FalseOptions -> String
showHelp opts | (displayHelp opts) = concat (intersperse "\n" helpText)
              | otherwise = ""

showVersion :: FalseOptions -> String
showVersion opts | (displayVersion opts) = concat (intersperse "\n" versionText)
                 | otherwise = ""

processArgs :: [String] -> FalseOptions -> FalseOptions
processArgs [] opts = opts
processArgs (x:xs) opts = case x of
  "--help" -> processArgs xs opts{displayHelp = True}
  "--version" -> processArgs xs opts{displayVersion = True}
  _ -> processArgs xs opts

stripQuotes :: String -> String
stripQuotes ('"':xs) = if last xs == '"' then init xs else ('"':xs)
stripQuotes xs = xs

defaultFalse :: FalseOptions
defaultFalse = FalseOptions False False

data FalseOptions = FalseOptions
  { displayHelp :: Bool
  , displayVersion :: Bool } deriving (Show, Eq)


helpText :: [String]
helpText = [ "Usage: /home/bminerds/x/coreutils/src/false [ignored command line arguments]"
             , "  or:  /home/bminerds/x/coreutils/src/false OPTION"
             , "Exit with a status code indicating failure."
             , "      --help     display this help and exit"
             , "      --version  output version information and exit"
             , "NOTE: your shell may have its own version of false, which usually supersedes"
             , "the version described here.  Please refer to your shell's documentation"
             , "for details about the options it supports."
             , "GNU coreutils online help: <http://www.gnu.org/software/coreutils/>"
             , "Full documentation at: <http://www.gnu.org/software/coreutils/false>"
             , "or available locally via: info '(coreutils) false invocation'\n"
           ]

versionText :: [String]
versionText = [ "Hfalse (Haskell implementation of GNU false) 1.0"
              , "derrived from: false (GNU coreutils) 8.23.126-99f76"
              , "Copyright (C) 2015 Free Software Foundation, Inc."
              , "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
              , "This is free software: you are free to change and redistribute it."
              , "There is NO WARRANTY, to the extent permitted by law."
              , "Written by Jim Meyering."
              , "Ported by PuZZleDucK.\n"
              ]

