-- Hshuf, a haskell implementation of GNU shuf.
module Main where
import System.Environment
import System.Console.Terminfo.Base
import Data.List
import GnUtils

--data ProgramOption = ProgramOption {
--  optionName :: String
--, optionShortFlags :: [String]
--, optionLongFlags :: [String]
--, optionParamaters :: [String]
--  --effect
--}

--data ProgramData = ProgramData {
--  appName :: String
--, appHelp :: String
--, appVersion :: String
--, argumentStrings :: [String]
--, argumentTokens :: [OptionToken]
--, configuration :: [ProgramOption]
--, longParser :: ProgramData -> String -> ProgramData
--, shortParser :: ProgramData -> String -> ProgramData
--}




main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs

  let shuf = ProgramData {
    appName = shufAppName
  , appHelp = shufAppHelp
  , appVersion = shufAppVersion
  , argumentStrings = args
  , argumentTokens = []
  , configuration = []
  , longParser = shufLongParser
  , shortParser = shufShortParser
  }
  let parsedShuf = parseArguments shuf args

--  let options = processArgs args defaultShuf

--  output <- showOutput options
--  runTermOutput term (termText (output))
--  runTermOutput term (termText ("Options: "++(show options)++"\n"))
  return ()




shufLongParser :: ProgramData -> String -> ProgramData
shufLongParser dat [] = dat
shufLongParser dat "shufflag" = dat

shufShortParser :: ProgramData -> String -> ProgramData
shufShortParser = (\x y -> x)

shufAppName = "Hshuf"
shufAppHelp = "help me shuffle"
shufAppVersion = "shuffle\n version\n"



showOutput :: ShufOptions -> IO String
showOutput opts = return "" -- <do-stuff-Here>






stripQuotes :: String -> String
stripQuotes ('"':xs) = if last xs == '"' then init xs else ('"':xs)
stripQuotes xs = xs

defaultShuf :: ShufOptions
defaultShuf = ShufOptions 

data ShufOptions = ShufOptions
  {  } deriving (Show, Eq)


helpText :: [String]
helpText = [ ""
           , "keep the newline->\n"
           ]

versionText :: [String]
versionText = [ "H<app-name> (Haskell implementation of GNU <app-name>) 1.0"
              , "derrived from: "
              , "Ported by PuZZleDucK.\n"
              ]

