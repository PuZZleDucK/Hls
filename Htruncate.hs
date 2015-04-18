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
  , configuration = customOptions
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




createOption :: Option
createOption = Option "do not create any files"
  (Flags ["c"] ["no-create"])
  (BoolOpt False)
  (OptionEffect (\(opts) _ -> replaceFlag opts "no-create" (BoolOpt True)))

blockOption = Option "treat SIZE as number of IO blocks instead of bytes"
  (Flags ["o"] ["io-blocks"])
  (BoolOpt False)
  (OptionEffect (\(opts) _ -> replaceFlag opts "io-blocks" (BoolOpt True)))

referenceOption = Option "base size on RFILE"
  (Flags ["r"] ["reference=RFILE"])
  (BoolOpt False)--TODO
  (OptionEffect (\(opts) _ -> replaceFlag opts "no-create" (BoolOpt True)))--TODO

sizeOption = Option "set or adjust the file size by SIZE bytes"
  (Flags ["s"] ["size=SIZE"])
  (BoolOpt False)--TODO
  (OptionEffect (\(opts) _ -> replaceFlag opts "no-create" (BoolOpt True)))--TODO

customOptions = catOptions defaultOptions (Options [
    createOption
  , sizeOption
  , referenceOption
  , blockOption
  ])







