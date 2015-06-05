-- Hyes, a haskell implementation of GNU yes.
module Main where
import System.Environment
import Data.List
import GUtils
import GOptions
import GFiles

main :: IO ()
main = do
  args <- getArgs

  let defaultConfig = ProgramData {
    appName = "Hyes"
  , appHelp = customHelp
  , appVersion = customVersion
  , argumentStrings = args
  , configuration = customOptions
  }
  let config = parseArguments defaultConfig args
  putStr (   showHelp config)
  putStr (showVersion config)
  
  let abort = (helpOrVersion config)
  if not abort then doWork config
               else return ()
--  putStrLn ("\n\n"++(show config)++"\n") --debug opts
  return ()



doWork :: ProgramData -> IO ()
doWork dat = do
--  putStrLn (concat targetList)--dbg
--  sequence_ (map (\x -> putStrLn (show x)) targetList)
    if ((length targetList) == 0)
      then putStrLn "y"
      else putStrLn (concat (intersperse " " targetList))
    doWork dat
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
customOptions = defaultOptions

customVersion :: String
customVersion = "Hyes, a Haskell clone of yes."
  ++ "\nderrived from: yes (GNU coreutils) 8.13"
  ++ "\nCopyright (C) 2011 Free Software Foundation, Inc."
  ++ "\nWritten by David MacKenzie and ported to Haskell by PuZZleDucK."

customHelp :: (String,String)
customHelp = ("Usage: Hyes [STRING]..."
              ++ "\nor:  Hyes [OPTION]"
              ++ "\nRepeatedly output a line with all specified STRING(s), or `y'."
             ,"\nReport Hyes bugs to PuZZleDucK+Hyes@gmail.com"
              ++ "\nGNU coreutils home page: <http://www.gnu.org/software/coreutils/>\n\n"
             )



