-- Hmkdir, a haskell implementation of GNU mkdir.
module Main where
import System.Environment
import GUtils

main :: IO ()
main = do
  args <- getArgs

  let defaultConfig = ProgramData {
    appName = "Hmkdir"
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
  putStrLn ("\n\n"++(show config)++"\n") --debug opts
  return ()



doWork :: ProgramData -> IO ()
doWork dat = do
--  putStrLn (concat targetList)--dbg
  sequence_ (map (\x -> putStrLn (show x)) targetList)
  sequence_ (map makeDirectory targetList)
    where cfg = configuration dat
          targets = getFlag "--" cfg
          targetList = getList targets



modeOption, parentOption, verboseOption, zOption, contextOption :: Option
modeOption = Option "set file mode (as in chmod), not a=rwx - umask"
  (Flags ["m"] ["mode"])
  "=MODE"
  (BoolOpt False)
  (OptionEffect (\(opts) _ unused -> ((replaceFlag opts "long-flag" (BoolOpt True)), unused)))
parentOption = Option "no error if existing, make parent directories as needed"
  (Flags ["p"] ["parents"])
  ""
  (BoolOpt False)
  (OptionEffect (\(opts) _ unused -> ((replaceFlag opts "parents" (BoolOpt True)), unused)))
verboseOption = Option "print a message for each created directory"
  (Flags ["v"] ["verbose"])
  "=<params>"
  (BoolOpt False)
  (OptionEffect (\(opts) _ unused -> ((replaceFlag opts "verbose" (BoolOpt True)), unused)))
zOption = Option "set SELinux security context of each created directory to the default type"
  (Flags ["Z"] [""])
  "=<params>"
  (BoolOpt False)
  (OptionEffect (\(opts) _ unused -> ((replaceFlag opts "Z" (BoolOpt True)), unused)))
contextOption = Option "like -Z, or if CTX is specified then set the SELinux or SMACK security context to CTX"
  (Flags [""] ["context"])
  "[=CTX]"
  (BoolOpt False)
  (OptionEffect (\(opts) _ unused -> ((replaceFlag opts "long-flag" (BoolOpt True)), unused)))

customOptions :: Options
customOptions = catOptions (Options [
  modeOption, parentOption, verboseOption, zOption, contextOption
  ]) defaultOptions

customVersion :: String
customVersion = "Hmkdir, a Haskell clone of mkdir."
  ++ "\nmkdir (GNU coreutils) 8.23.126-99f76"
  ++ "\nCopyright (C) 2015 Free Software Foundation, Inc."
  ++ "\nLicense GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
  ++ "\nThis is free software: you are free to change and redistribute it. There is NO WARRANTY, to the extent permitted by law."
  ++ "\nWritten by David MacKenzie."
  ++ "\nPorted to Haskell by PuZZleDucK."

customHelp :: (String,String)
customHelp = ("Usage: H<app> ...."
              ++ "\nUsage: /home/bminerds/x/coreutils/src/mkdir [OPTION]... DIRECTORY..."
              ++ "\nCreate the DIRECTORY(ies), if they do not already exist."
              ++ "\nMandatory arguments to long options are mandatory for short options too."
             , "\nGNU coreutils online help: <http://www.gnu.org/software/coreutils/>"
              ++ "\nFull documentation at: <http://www.gnu.org/software/coreutils/mkdir> or available locally via: info '(coreutils) mkdir invocation'"
             )



