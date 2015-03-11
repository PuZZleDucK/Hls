-- Lets try something a little less ambitious, like whoami.
module Main where
import System.Environment
import System.Console.Terminfo.Base
import Data.List
import Control.Monad

main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs
  let options = processArgs args defaultWhoami
--  runTermOutput term (termText ("Options: "++(show options)++"\n"))

  runTermOutput term (termText (showHelp options))
  runTermOutput term (termText (showVersion options))
  o <- showOutput options
  runTermOutput term (termText (o))
  return ()

showOutput :: WhoamiOptions -> IO String
showOutput opts | not ((displayHelp opts) || (displayVersion opts)) = getUsername
                | otherwise = return ""

getUsername :: IO String
getUsername = do e <- getEnvironment
                 return (((snd . head) (filter (\x -> (fst x)=="USER") e))++"\n")


showHelp :: WhoamiOptions -> String
showHelp opts | (displayHelp opts) = concat (intersperse "\n" helpText)
              | otherwise = ""

showVersion :: WhoamiOptions -> String
showVersion opts | (displayVersion opts) = concat (intersperse "\n" versionText)
                 | otherwise = ""

processArgs :: [String] -> WhoamiOptions -> WhoamiOptions
processArgs [] opts = opts
processArgs (x:xs) opts = case x of
  "--help" -> processArgs xs opts{displayHelp = True}
  "--version" -> processArgs xs opts{displayVersion = True}
  _ -> processArgs xs opts

stripQuotes :: String -> String
stripQuotes ('"':xs) = if last xs == '"' then init xs else ('"':xs)
stripQuotes xs = xs

defaultWhoami :: WhoamiOptions
defaultWhoami = WhoamiOptions False False

data WhoamiOptions = WhoamiOptions { displayHelp :: Bool
                                , displayVersion :: Bool } deriving (Show, Eq)


helpText :: [String]
helpText = [ "Usage: whoami [OPTION]..."
           , "Print the user name associated with the current effective user ID."
           , "Same as id -un."
           , "      --help display this help and exit"
           , "      --version output version information and exit"
           , "GNU coreutils online help: <http://www.gnu.org/software/coreutils/>"
           , "For complete documentation, run: info coreutils 'whoami invocation'\n"
           ]

versionText :: [String]
versionText = [ "Hwhoami (Haskell implementation of GNU whoami) 1.0"
              , "derrived from: whoami (GNU coreutils) 8.23"
              , "Copyright (C) 2014 Free Software Foundation, Inc."
              , "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
              , "This is free software: you are free to change and redistribute it."
              , "There is NO WARRANTY, to the extent permitted by law."
              , "Written by Richard Mlynarik."
              , "Ported by PuZZleDucK.\n"
              ]


