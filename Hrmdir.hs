-- Hrmdir, a haskell implementation of GNU rmdir.
module Main where
import System.Environment
import GUtils
import GFiles
import GOptions

main :: IO ()
main = do
  args <- getArgs

  let defaultConfig = ProgramData {
    appName = "Hrmdir"
  , appHelp = customHelp
  , appVersion = customVersion
  , argumentStrings = args
  , configuration = customOptions
  , parser = undefined -- needed anymore??
  }
  let config = parseArguments defaultConfig args
  putStrLn (   showHelp config)
  putStrLn (showVersion config)
  
  doWork config
--  putStrLn ("\n\n"++(show config)++"\n") --debug opts
  return ()



doWork :: ProgramData -> IO ()
doWork dat = do
  directoryList <- onlyDirs targetList
  emptyList <- onlyEmptyDirs directoryList
  sequence_ (map deleteDir emptyList)
    where cfg = configuration dat
          targets = getFlag "--" cfg
          targetList = getList targets

ignoreOption, parentsOption, verboseOption :: Option
ignoreOption = Option "ignore each failure that is solely because a directory is non-empty"
  (Flags [""] ["ignore-fail-on-non-empty"])
  ""
  (BoolOpt False)
  (OptionEffect (\(opts) _ unused -> ((replaceFlag opts "ignore-fail-on-non-empty" (BoolOpt True)), unused)))
parentsOption = Option "remove DIRECTORY and its ancestors; e.g., 'rmdir -p a/b/c' is similar to 'rmdir a/b/c a/b a'"
  (Flags ["p"] ["parents"])
  ""
  (BoolOpt False)
  (OptionEffect (\(opts) _ unused -> ((replaceFlag opts "parents" (BoolOpt True)), unused)))
verboseOption = Option "output a diagnostic for every directory processed"
  (Flags ["v"] ["verbose"])
  ""
  (BoolOpt False)
  (OptionEffect (\(opts) _ unused -> ((replaceFlag opts "verbose" (BoolOpt True)), unused)))

customOptions :: Options
customOptions = catOptions (Options [
  ignoreOption, parentsOption, verboseOption
  ]) defaultOptions

customVersion :: String
customVersion = "Hrmdir, a Haskell clone of <app>."
  ++ "\nrmdir (GNU coreutils) 8.23.126-99f76"
  ++ "\nCopyright (C) 2015 Free Software Foundation, Inc."
  ++ "\nLicense GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
  ++ "\nThis is free software: you are free to change and redistribute it. There is NO WARRANTY, to the extent permitted by law."
  ++ "\nWritten by David MacKenzie."
  ++ "\nPorted to Haskell by PuZZleDucK."

customHelp :: (String,String)
customHelp = ("Usage: Hrmdir ...."
              ++ "\nUsage: /home/bminerds/x/coreutils/src/rmdir [OPTION]... DIRECTORY..."
              ++ "\nRemove the DIRECTORY(ies), if they are empty."
             ,
             "\nGNU coreutils online help: <http://www.gnu.org/software/coreutils/>"
              ++ "\nFull documentation at: <http://www.gnu.org/software/coreutils/rmdir>"
              ++ "\nor available locally via: info '(coreutils) rmdir invocation'"
             )



