-- Lets try something a little less ambitious, like yes.
module Main where
import System.Environment
import System.Console.Terminfo.Base
import Data.List

main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs
  let options = processArgs args defaultOptions
--  runTermOutput term (termText ("Options: "++(show options)++"\n"))

  runTermOutput term (termText (showHelp options))
  runTermOutput term (termText (showVersion options))
  runTermOutput term (termText (showOutput options)) --if not (help or ver)?
  return ()

showOutput :: YesOptions -> String
showOutput opts | not ((displayHelp opts) || (displayVersion opts)) = (displayString opts)++"\n"++(showOutput opts)
                | otherwise = ""

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
           , "or:  yes [OPTION]"
           , "Repeatedly output a line with all specified STRING(s), or `y'."
           , "  --help     display this help and exit"
           , "  --version  output version information and exit"
           , "Report Hyes bugs to PuZZleDucK+Hyes@gmail.com"
           , "GNU coreutils home page: <http://www.gnu.org/software/coreutils/>\n\n"
           ]

versionText :: [String]
versionText = [ "Hyes (Haskell implementation of GNU yes) 1.0"
              , "derrived from: yes (GNU coreutils) 8.13"
              , "Copyright (C) 2011 Free Software Foundation, Inc."
              , "Written by David MacKenzie."
              , "Ported by PuZZleDucK.\n\n"
              ]




-- Just performed a quick & dirty comparison running yes and Hyes
-- 1 puzzleduck puzzleduck 225931484 Feb 26 18:35 10comp.txt
-- 1 puzzleduck puzzleduck 368005566 Feb 27 02:37 10fast.txt -- half way already  :D
-- 1 puzzleduck puzzleduck 718757888 Feb 26 18:28 10gnu.txt
-- 1 puzzleduck puzzleduck  16453786 Feb 26 18:28 10.txt



