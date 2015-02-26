-- Lets try something a little less ambitious, like yes.
module Main where
import System.Environment
import System.Console.Terminfo.Base
import Data.List

main :: IO ()
main = do
--  putStrLn (" :: Haskell yes :: ")
  term <- setupTermFromEnv
  args <- getArgs
  let options = processArgs args defaultOptions

--  runTermOutput term (termText ("Options: "++(show options)++"\n"))

  runTermOutput term (termText (showHelp options))
  runTermOutput term (termText (showVersion options))
  runTermOutput term (termText (showOutput options))
  return ()

showOutput :: YesOptions -> String
showOutput opts = (displayString opts)++"\n"++(showOutput opts)

showHelp :: YesOptions -> String
showHelp opts | (displayHelp opts) = concat (intersperse "\n" helpText)
              | otherwise = ""

showVersion :: YesOptions -> String
showVersion opts | (displayVersion opts) = concat (intersperse "\n" versionText)
                 | otherwise = ""

processArgs :: [String] -> YesOptions -> YesOptions
processArgs [] opts = if (displayString opts) == ""
  then opts{displayString = "y"}
  else opts{displayString = stripQuotes (displayString opts)}
processArgs (x:xs) opts = case x of
  "--help" -> processArgs xs opts{displayHelp = True}
  "--version" -> processArgs xs opts{displayVersion = True}
  _ -> if priorString == ""
    then processArgs xs opts{displayString = x}
    else processArgs xs opts{displayString = priorString++" "++x}
      where priorString = displayString opts

stripQuotes :: String -> String
stripQuotes ('"':xs) = if last xs == '"' then init xs else ('"':xs)
stripQuotes xs = xs

defaultOptions :: YesOptions 
defaultOptions = YesOptions "" False False

data YesOptions = YesOptions { displayString :: String
                             , displayHelp :: Bool
                             , displayVersion :: Bool } deriving (Show, Eq)


helpText :: [String]
helpText = [ "Usage: yes [STRING]..."
           , "or:  yes OPTION"
           , "Repeatedly output a line with all specified STRING(s), or `y'."
           , "  --help     display this help and exit"
           , "  --version  output version information and exit"
           , "Report yes bugs to bug-coreutils@gnu.org"
           , "GNU coreutils home page: <http://www.gnu.org/software/coreutils/>"
           , "General help using GNU software: <http://www.gnu.org/gethelp/>"
           ]

versionText :: [String]
versionText = [ "yes (GNU coreutils) 8.13"
              , "Copyright (C) 2011 Free Software Foundation, Inc."
              , "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
              , "This is free software: you are free to change and redistribute it."
              , "There is NO WARRANTY, to the extent permitted by law."
              , "Written by David MacKenzie."
              ]



