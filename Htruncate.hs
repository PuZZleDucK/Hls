-- Htruncate, a haskell implementation of GNU truncate.
module Main where
import System.Environment
import System.Console.Terminfo.Base
--import Data.List
import GUtils

main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs

  let defaultConfig = ProgramData {
    appName = "Htruncate"
  , appHelp = ("appHelpPre","appHelpPost")
  , appVersion = "appVersion"
  , argumentStrings = args
  , configuration = defaultOptions
  , parser = undefined
--  , oParser = longOptionParser
  }
  let config = parseArguments defaultConfig args
  runTermOutput term (termText ("getHelp config"))
  runTermOutput term (termText ("getVersion config"))
  
  --not very conducive to REPL loop refactoring... and do help first
  --interact (showOutput config (getContents input-target)) output-target
  input <- getContents
--  output <- getOutput config input
  runTermOutput term (termText ("output"))
  runTermOutput term (termText ("\n"++(show config)++"\n")) --debug opts
  return ()





