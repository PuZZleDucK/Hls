-- Hpwd, a haskell implementation of GNU pwd.
module Main where
import System.Environment
import System.Console.Terminfo.Base
import Data.List

main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs
  let options = processArgs args defaultPwd
  runTermOutput term (termText ("Options: "++(show options)++"\n"))

  runTermOutput term (termText (showHelp options))
  runTermOutput term (termText (showVersion options))
  output <- showOutput options
  runTermOutput term (termText (output))
  return ()

showOutput :: PwdOptions -> IO String
showOutput opts | not ((displayHelp opts) || (displayVersion opts)) = return "" -- <do-stuff-Here>
                | otherwise = return ""


showHelp :: PwdOptions -> String
showHelp opts | (displayHelp opts) = concat (intersperse "\n" helpText)
              | otherwise = ""

showVersion :: PwdOptions -> String
showVersion opts | (displayVersion opts) = concat (intersperse "\n" versionText)
                 | otherwise = ""

processArgs :: [String] -> PwdOptions -> PwdOptions
processArgs [] opts = opts
processArgs (x:xs) opts = case x of
  "--help" -> processArgs xs opts{displayHelp = True}
  "--version" -> processArgs xs opts{displayVersion = True}
  "--" -> opts --GNU extension, terminate option parsing, still need to parse targets
  _ -> processArgs xs opts -- if POSIXLY_CORRECT: processTargets xs opts
                           -- as POSIX terminates option processing after the first target

stripQuotes :: String -> String
stripQuotes ('"':xs) = if last xs == '"' then init xs else ('"':xs)
stripQuotes xs = xs

defaultPwd :: PwdOptions
defaultPwd = PwdOptions False False

data PwdOptions = PwdOptions
  { displayHelp :: Bool
  , displayVersion :: Bool } deriving (Show, Eq)


helpText :: [String]
helpText = [ "Usage: /home/bminerds/x/coreutils/src/pwd [OPTION]..."
           , "Print the full filename of the current working directory."
           , "  -L, --logical   use PWD from environment, even if it contains symlinks"
           , "  -P, --physical  avoid all symlinks"
           , "      --help     display this help and exit"
           , "      --version  output version information and exit"
           , "If no option is specified, -P is assumed."
           , "NOTE: your shell may have its own version of pwd, which usually supersedes"
           , "the version described here.  Please refer to your shell's documentation"
           , "for details about the options it supports."
           , "GNU coreutils online help: <http://www.gnu.org/software/coreutils/>"
           , "Full documentation at: <http://www.gnu.org/software/coreutils/pwd>"
           , "or available locally via: info '(coreutils) pwd invocation'\n"
           ]

versionText :: [String]
versionText = [ "H<app-name> (Haskell implementation of GNU <app-name>) 1.0"
              , "derrived from: pwd (GNU coreutils) 8.23.126-99f76"
              , "Copyright (C) 2015 Free Software Foundation, Inc."
              , "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
              , "This is free software: you are free to change and redistribute it."
              , "There is NO WARRANTY, to the extent permitted by law."
              , "Written by Jim Meyering."
              , "Ported by PuZZleDucK.\n"
              ]

