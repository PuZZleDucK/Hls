-- Htrue, a haskell implementation of GNU true.
module Main where
import System.Environment
import System.Console.Terminfo.Base
import Data.List

main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs
  let options = processArgs args defaultTrue
--  runTermOutput term (termText ("Options: "++(show options)++"\n"))

  runTermOutput term (termText (showHelp options))
  runTermOutput term (termText (showVersion options))
  return () --the definition of success

showHelp :: TrueOptions -> String
showHelp opts | (displayHelp opts) = concat (intersperse "\n" helpText)
              | otherwise = ""

showVersion :: TrueOptions -> String
showVersion opts | (displayVersion opts) = concat (intersperse "\n" versionText)
                 | otherwise = ""

processArgs :: [String] -> TrueOptions -> TrueOptions
processArgs [] opts = opts
processArgs (x:xs) opts = case x of
  "--help" -> processArgs xs opts{displayHelp = True}
  "--version" -> processArgs xs opts{displayVersion = True}
  _ -> processArgs xs opts

stripQuotes :: String -> String
stripQuotes ('"':xs) = if last xs == '"' then init xs else ('"':xs)
stripQuotes xs = xs

defaultTrue :: TrueOptions
defaultTrue = TrueOptions False False

data TrueOptions = TrueOptions
  { displayHelp :: Bool
  , displayVersion :: Bool } deriving (Show, Eq)


helpText :: [String]
helpText = [ "Usage: /home/bminerds/x/coreutils/src/true [ignored command line arguments]"
           , "  or:  /home/bminerds/x/coreutils/src/true OPTION"
           , "Exit with a status code indicating success."
           , "      --help     display this help and exit"
           , "      --version  output version information and exit"
           , "NOTE: your shell may have its own version of true, which usually supersedes"
           , "the version described here.  Please refer to your shell's documentation"
           , "for details about the options it supports."
           , "GNU coreutils online help: <http://www.gnu.org/software/coreutils/>"
           , "Full documentation at: <http://www.gnu.org/software/coreutils/true>"
           , "or available locally via: info '(coreutils) true invocation'\n"
           ]

versionText :: [String]
versionText = [ "Htrue (Haskell implementation of GNU true) 1.0"
              , "derrived from: true (GNU coreutils) 8.23.126-99f76"
              , "Copyright (C) 2015 Free Software Foundation, Inc."
              , "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
              , "This is free software: you are free to change and redistribute it."
              , "There is NO WARRANTY, to the extent permitted by law."
              , "Written by Jim Meyering."
              , "Ported by PuZZleDucK.\n"
              ]

