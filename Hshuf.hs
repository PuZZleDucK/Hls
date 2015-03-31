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
  , configuration = defaultConfig
  , longParser = shufLongParser
  , shortParser = shufShortParser
  }
  let parsedShuf = parseArguments shuf args

--  output <- showOutput options
--  runTermOutput term (termText (output))
  if doHelp parsedShuf
    then runTermOutput term (termText (getHelp parsedShuf))
    else return ()
  if doVersion parsedShuf
    then runTermOutput term (termText (getVersion parsedShuf))
    else return ()
  runTermOutput term (termText ("\n"++(show parsedShuf)++"\n"))
  return ()



defaultConfig = ConfigurationData defaultOptions [] [] []

shufLongParser :: ConfigurationData -> String -> ConfigurationData
shufLongParser dat [] = dat
shufLongParser dat "zero-terminated" = dat
shufLongParser dat "repeat" = dat
shufLongParser dat "random-source" = dat -- =FILE
shufLongParser dat "output" = dat -- =FILE
shufLongParser dat "head-count" = dat -- =COUNT
shufLongParser dat "input-range" = dat -- =LO-HI
shufLongParser dat "echo" = dat

shufShortParser :: ConfigurationData -> String -> ConfigurationData
shufShortParser = (\x y -> x) -- z r o=FILE n=COUNT i=LO-HI e

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
helpText = [ "Usage: /home/bminerds/x/coreutils/src/shuf [OPTION]... [FILE]"
           , "  or:  /home/bminerds/x/coreutils/src/shuf -e [OPTION]... [ARG]..."
           , "  or:  /home/bminerds/x/coreutils/src/shuf -i LO-HI [OPTION]..."
           , "Write a random permutation of the input lines to standard output."
           , "Mandatory arguments to long options are mandatory for short options too."
           , "  -e, --echo                treat each ARG as an input line"
             , "-i, --input-range=LO-HI   treat each number LO through HI as an input line"
           , "  -n, --head-count=COUNT    output at most COUNT lines"
           , "  -o, --output=FILE         write result to FILE instead of standard output"
           , "      --random-source=FILE  get random bytes from FILE"
           , "  -r, --repeat              output lines can be repeated"
           , "  -z, --zero-terminated     line delimiter is NUL, not newline"
           , "      --help     display this help and exit"
           , "      --version  output version information and exit"
           , "With no FILE, or when FILE is -, read standard input."
           , "GNU coreutils online help: <http://www.gnu.org/software/coreutils/>"
           , "Full documentation at: <http://www.gnu.org/software/coreutils/shuf>"
           , "or available locally via: info '(coreutils) shuf invocation'\n"
           ]

versionText :: [String]
versionText = [ "H<app-name> (Haskell implementation of GNU <app-name>) 1.0"
              , "derrived from: "
              , "Ported by PuZZleDucK.\n"
              ]

