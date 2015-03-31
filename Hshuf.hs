-- Hshuf, a haskell implementation of GNU shuf.
module Main where
import System.Environment
import System.Console.Terminfo.Base
import Data.List
import GnUtils

--data ProgramData = ProgramData {
--  appName :: String
--, appHelp :: String
--, appVersion :: String
--, argumentStrings :: [String]
--, configuration :: ConfigurationData
--, longParser :: ProgramData -> String -> ProgramData
--, shortParser :: ProgramData -> String -> ProgramData
--}

--data ConfigurationData = ConfigurationData {
--  boolData :: [ProgramOption Bool]
--, stringData :: [ProgramOption String]
--, integerData :: [ProgramOption Integer]
--, floatData :: [ProgramOption Float]
--} deriving (Show)

shufDumm = ProgramData "shuf"
                       ("help","text")
                       "version text"
                       [] -- args
                       (ConfigurationData defaultOptions [] [] []) --cfg
                       (\x y -> x)
                       (\x y -> x)

main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs

  let shuf = ProgramData {
    appName = shufAppName
  , appHelp = (shufAppHelpPre,shufAppHelpPost)
  , appVersion = shufAppVersion
  , argumentStrings = args
  , configuration = ConfigurationData defaultOptions [] [] []
  , longParser = shufLongParser
  , shortParser = shufShortParser
  }
--  runTermOutput term (termText ((show shuf)++"\n"))
  let parsedShuf = parseArguments shuf args

--  let options = processArgs args defaultShuf

--  output <- showOutput options
--  runTermOutput term (termText (output))
  if doHelp shuf
    then runTermOutput term (termText (getHelp shuf))
    else return ()
  if doVersion shuf
    then runTermOutput term (termText (getVersion shuf))
    else return ()
  runTermOutput term (termText ((show parsedShuf)++"\n"))
  return ()




shufLongParser :: ConfigurationData -> String -> ConfigurationData
shufLongParser dat [] = dat
shufLongParser dat "shufflag" = dat

shufShortParser :: ConfigurationData -> String -> ConfigurationData
shufShortParser = (\x y -> x)

shufAppName = "Hshuf"
shufAppHelpPre = "help me shuffle..."
shufAppHelpPost = "...done"
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

