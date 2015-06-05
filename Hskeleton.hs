-- H<app>, a haskell implementation of GNU <app>.
module Main where
import System.Environment
import GUtils
import GOptions
import GFiles

main :: IO ()
main = do
  args <- getArgs

  let defaultConfig = ProgramData {
    appName = "H<app>"
  , appHelp = customHelp
  , appVersion = customVersion
  , argumentStrings = args
  , configuration = customOptions
  }
  let config = parseArguments defaultConfig args
  putStr (   showHelp config)
  putStr (showVersion config)
  
  let abort = (helpOrVersion config) || (length (getTargets (configuration config))) == 0
  if not abort then doWork config
               else return ()
  putStrLn ("\n\n"++(show config)++"\n") --debug opts
  return ()



doWork :: ProgramData -> IO ()
doWork dat = do
--  putStrLn (concat targetList)--dbg
  sequence_ (map (\x -> putStrLn (show x)) targetList)
    where cfg = configuration dat
          (Just targets) = getFlag "--" cfg
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
customVersion = "H<app>, a Haskell clone of <app>."
  ++ "\n"
  ++ "\n"
  ++ "\nPorted to Haskell by PuZZleDucK."

customHelp :: (String,String)
customHelp = ("Usage: H<app> ...."
              ++ "\n"
              ++ "\n"
             ,"\n"
              ++ "\n"
             )



