-- Htruncate, a haskell implementation of GNU truncate.
module Main where
import System.Environment
import GUtils

main :: IO ()
main = do
  args <- getArgs

  let defaultConfig = ProgramData {
    appName = "Htruncate"
  , appHelp = customHelp
  , appVersion = customVersion
  , argumentStrings = args
  , configuration = customOptions
  , parser = undefined
  }
  let config = parseArguments defaultConfig args
  putStrLn (   showHelp config)
  putStrLn (showVersion config)
  
  --not very conducive to REPL loop refactoring... and do help first
  --interact (showOutput config (getContents input-target)) output-target
--  input <- getContents
  doWork config
--  runTermOutput term (termText (output))
  putStrLn ("\n\n"++(show config)++"\n") --debug opts
  return ()



doWork :: ProgramData -> IO ()
doWork dat = do
  putStrLn (concat targetList)--dbg
  size <- calculateSize (getSize sizeFlag) (getString refFlag)
  case size of
     (Left err) -> putStrLn (err)--dbg
     (Right s) -> putStrLn ("S:"++(show s))--dbg
--  return "OUTPUT"
  putStrLn "\nNow work:\n"
  sequence_ (map (\x -> putStrLn (show x)) targetList)
  case pre of (Extend) -> putStrLn "\nCaseWork +\n"
              (Reduce) -> putStrLn "\nCaseWork -\n"
              (AtMost) -> putStrLn "\nCaseWork >\n"
              (AtLeast) -> putStrLn "\nCaseWork <\n"
              (RoundDown) -> putStrLn "\nCaseWork \\ \n"
              (RoundUp) -> putStrLn "\nCaseWork % \n"
              (NoPrefix) -> putStrLn "\nCaseWork Absolute \n"
    where cfg = configuration dat
          targets = getFlag "--" cfg
          targetList = getList targets
          sizeFlag = getFlag "size" cfg
          refFlag = getFlag "reference" cfg
          (GnuSize pre _num _unt) = getSize sizeFlag
          
          

calculateSize :: GnuSize -> String -> IO (Either String Integer)
calculateSize (GnuSize _ (-1) _) "" = return (Left noSizeError)
calculateSize size "" = return (Right (calcGnuSize size))
calculateSize (GnuSize _ (-1) _) x = do 
  fs <- getFileSize x
  return (Right (fs))
calculateSize (GnuSize Extend x y) f = do 
  fs <- getFileSize f
  let ex = calcGnuSize (GnuSize Extend x y)
  return (Right (fs+ex))--size plus ref
calculateSize (GnuSize Reduce x y) f = do 
  fs <- getFileSize f
  let ex = calcGnuSize (GnuSize Reduce x y)
  return (Right (fs-ex))--size plus ref
calculateSize _ _ = return (Left "Unknown error.")


  


--System.Directory.doesFileExist
--System.FilePath.Posix.isValid

noSizeError, relativeSizeError :: String
noSizeError = "you must specify either ‘--size’ or ‘--reference’\n"
relativeSizeError = "you must specify a relative ‘--size’ with ‘--reference’\n"

createOption, blockOption, referenceOption, sizeOption :: Option
createOption = Option "do not create any files"
  (Flags ["c"] ["no-create"])
  ""
  (BoolOpt False)
  (OptionEffect (\(opts) _ unused -> ((replaceFlag opts "no-create" (BoolOpt True)), unused)))

blockOption = Option "treat SIZE as number of IO blocks instead of bytes"
  (Flags ["o"] ["io-blocks"])
  ""
  (BoolOpt False)
  (OptionEffect (\(opts) _ unused -> (replaceFlag opts "io-blocks" (BoolOpt True), unused)))

referenceOption = Option "base size on RFILE"
  (Flags ["r"] ["reference"])
  "=RFILE"
  (StringOpt "")
  (OptionEffect (\(opts) this rest -> let (newRef,newUnused) = parseOptionFileName (this:rest) in (replaceFlag opts "reference" (StringOpt newRef),newUnused)))

sizeOption = Option "set or adjust the file size by SIZE bytes"
  (Flags ["s"] ["size"])
  "=SIZE"
  (GnuSizeOpt (GnuSize NoPrefix (-1) NoUnits))
  (OptionEffect (\(opts) this rest -> let (newSize,newUnused) = parseOptionSize (this:rest) in (replaceFlag opts "s" (GnuSizeOpt newSize), newUnused)))--TODO -- parseOptionSize

customOptions :: Options
customOptions = catOptions (Options [
    createOption
  , sizeOption
  , referenceOption
  , blockOption
  ]) defaultOptions

customVersion :: String
customVersion = "Htruncate, a Haskell clone of truncate."
  ++ "\ntruncate (GNU coreutils) 8.23.126-99f76"
  ++ "\nCopyright (C) 2015 Free Software Foundation, Inc."
  ++ "\nLicense GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
  ++ "\nThis is free software: you are free to change and redistribute it."
  ++ "\nThere is NO WARRANTY, to the extent permitted by law."
  ++ "\nWritten by Pádraig Brady."
  ++ "\nPorted to Haskell by PuZZleDucK."

customHelp :: (String,String)
customHelp = ("Usage: Htruncate OPTION... FILE..."
              ++ "\nShrink or extend the size of each FILE to the specified size"
              ++ "\n\nA FILE argument that does not exist is created."
              ++ "\n\nIf a FILE is larger than the specified size, the extra data is lost."
              ++ "\nIf a FILE is shorter, it is extended and the extended part (hole) reads as zero bytes."
              ++ "\n\nMandatory arguments to long options are mandatory for short options too."
  ,"\nThe SIZE argument is an integer and optional unit (example: 10K is 10*1024)."
              ++ "\nUnits are K,M,G,T,P,E,Z,Y (powers of 1024) or KB,MB,... (powers of 1000)."
              ++ "\nSIZE may also be prefixed by one of the following modifying characters:"
              ++ "\n'+' extend by, '-' reduce by, '<' at most, '>' at least, '/' round down to multiple of, '%' round up to multiple of."
              ++ "\n\nGNU coreutils online help: <http://www.gnu.org/software/coreutils/>"
              ++ "\nFull documentation at: <http://www.gnu.org/software/coreutils/truncate> or available locally via: info '(coreutils) truncate invocation'"
  )



