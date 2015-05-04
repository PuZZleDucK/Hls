-- Hrmdir, a haskell implementation of GNU rmdir.
module Main where
import System.Environment
import GUtils

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
  putStrLn ("\n\n"++(show config)++"\n") --debug opts
  return ()



doWork :: ProgramData -> IO ()
doWork dat = do
--  putStrLn (concat targetList)--dbg
  sequence_ (map (\x -> putStrLn (show x)) targetList)
    where cfg = configuration dat
          targets = getFlag "--" cfg
          targetList = getList targets

someOption :: Option
someOption = Option "<Op text>"
  (Flags ["s"] ["long-flag"])
  "=<params>"
  (BoolOpt False)
  (OptionEffect (\(opts) _ unused -> ((replaceFlag opts "long-flag" (BoolOpt True)), unused)))

customOptions :: Options
customOptions = catOptions (Options [
  someOption
  ]) defaultOptions

customVersion :: String
customVersion = "Hrmdir, a Haskell clone of <app>."
  ++ "\n"
  ++ "\n"
  ++ "\nPorted to Haskell by PuZZleDucK."

customHelp :: (String,String)
customHelp = ("Usage: Hrmdir ...."
              ++ "\n"
              ++ "\n"
             ,"\n"
              ++ "\n"
             )



