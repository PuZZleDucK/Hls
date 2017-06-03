-- Husers, a haskell implementation of GNU users.
module Main where
import System.Environment
import System.Console.Terminfo.Base
import Data.List

main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs
  let options = processArgs args defaultUsers
--  runTermOutput term (termText ("Options: "++(show options)++"\n"))

  runTermOutput term (termText (showHelp options))
  runTermOutput term (termText (showVersion options))
  --needs to possibly process a file instead of querying system.
  output <- showOutput options
  runTermOutput term (termText output)
  return ()

showOutput :: UsersOptions -> IO String
showOutput opts | not (displayHelp opts) || (displayVersion opts) = return "" -- <do-stuff-Here>
                | otherwise = return ""


showHelp :: UsersOptions -> String
showHelp opts | displayHelp opts =   intercalate "\n" helpText
              | otherwise = ""

showVersion :: UsersOptions -> String
showVersion opts | displayVersion opts =   intercalate "\n" versionText
                 | otherwise = ""

processArgs :: [String] -> UsersOptions -> UsersOptions
processArgs [] opts = opts
processArgs (x:xs) opts = case x of
  "--help" -> processArgs xs opts{displayHelp = True}
  "--version" -> processArgs xs opts{displayVersion = True}
  _ -> processArgs xs opts

stripQuotes :: String -> String
stripQuotes ('"':xs) = if last xs == '"' then init xs else '"' : xs
stripQuotes xs = xs

defaultUsers :: UsersOptions
defaultUsers = UsersOptions False False

data UsersOptions = UsersOptions
  { displayHelp :: Bool
  , displayVersion :: Bool } deriving (Show, Eq)


helpText :: [String]
helpText = [ "Usage: users [OPTION]... [FILE]"
           , "Output who is currently logged in according to FILE."
           , "If FILE is not specified, use /var/run/utmp.  /var/log/wtmp as FILE is common."
           , "      --help     display this help and exit"
           , "      --version  output version information and exit"
           , "Report users bugs to bug-coreutils@gnu.org"
           , "GNU coreutils home page: <http://www.gnu.org/software/coreutils/>"
           , "General help using GNU software: <http://www.gnu.org/gethelp/>"
           , "For complete documentation, run: info coreutils 'users invocation'\n"
           ]

versionText :: [String]
versionText = [ "H<app-name> (Haskell implementation of GNU <app-name>) 1.0"
              , "derrived from: users (GNU coreutils) 8.13"
              , "Copyright (C) 2011 Free Software Foundation, Inc."
              , "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
              , "This is free software: you are free to change and redistribute it."
              , "There is NO WARRANTY, to the extent permitted by law."
              , "Written by Joseph Arceneaux and David MacKenzie."
              , "Ported by PuZZleDucK.\n"
              ]

