-- Htruncate, a haskell implementation of GNU truncate.
module Main where
import System.Environment
import System.Console.Terminfo.Base
import GUtils

main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs

  let defaultConfig = ProgramData {
    appName = "Htruncate"
  , appHelp = customHelp
  , appVersion = "appVersion"
  , argumentStrings = args
  , configuration = customOptions
  , parser = undefined
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
  (OptionEffect (\(opts) _ unused -> ((replaceFlag opts "no-create" (BoolOpt True)), unused)))

blockOption = Option "treat SIZE as number of IO blocks instead of bytes"
  (Flags ["o"] ["io-blocks"])
  (BoolOpt False)
  (OptionEffect (\(opts) _ unused -> (replaceFlag opts "io-blocks" (BoolOpt True), unused)))

referenceOption = Option "base size on RFILE"
  (Flags ["r"] ["reference"])
  (StringOpt "")
  (OptionEffect (\(opts) this rest -> let (newRef,newUnused) = parseOptionFileName (this:rest) in (replaceFlag opts "reference" (StringOpt newRef),newUnused))) --TODOing

sizeOption = Option "set or adjust the file size by SIZE bytes"
  (Flags ["s"] ["size=SIZE"])
  (BoolOpt False)--TODO
  (OptionEffect (\(opts) _ unused -> (replaceFlag opts "no-create" (BoolOpt True), unused)))--TODO

customOptions = catOptions defaultOptions (Options [
    createOption
  , sizeOption
  , referenceOption
  , blockOption
  ])

customHelp :: (String,String)
customHelp = ("\nUsage: truncate OPTION... FILE..."
              ++ "\nShrink or extend the size of each FILE to the specified size"
              ++ "\nA FILE argument that does not exist is created."
              ++ "\nIf a FILE is larger than the specified size, the extra data is lost."
              ++ "\nIf a FILE is shorter, it is extended and the extended part (hole)"
              ++ "\nreads as zero bytes."
              ++ "\nMandatory arguments to long options are mandatory for short options too."
  ,"The SIZE argument is an integer and optional unit (example: 10K is 10*1024)."
              ++ "\nUnits are K,M,G,T,P,E,Z,Y (powers of 1024) or KB,MB,... (powers of 1000)."
              ++ "\nSIZE may also be prefixed by one of the following modifying characters:"
              ++ "\n'+' extend by, '-' reduce by, '<' at most, '>' at least,"
              ++ "\n'/' round down to multiple of, '%' round up to multiple of."
              ++ "\nGNU coreutils online help: <http://www.gnu.org/software/coreutils/>"
              ++ "\nFull documentation at: <http://www.gnu.org/software/coreutils/truncate>"
              ++ "\nor available locally via: info '(coreutils) truncate invocation'"
  )





