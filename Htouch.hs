-- Htouch, a haskell implementation of GNU touch.
module Main where
import System.Environment
import System.Console.Terminfo.Base
import Data.List
import Text.Parsec
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs
--  let opts = parse parseFlags defaultTouch args
  --let opts2 = parse opts
-- usage: parse parseHex [] "x41stuff"
  let opts = parseFlags args
  let options = processArgs args defaultTouch
  runTermOutput term (termText ("Options: "++(show options)++"\n"))
--  runTermOutput term (termText ("Options: "++(show opts)++"\n"))  -- not working

  runTermOutput term (termText (showHelp options))
  runTermOutput term (termText (showVersion options))
  output <- showOutput options
  runTermOutput term (termText (output))
  return ()

showOutput :: TouchOptions -> IO String
showOutput opts | not ((displayHelp opts) || (displayVersion opts)) = return "" -- <do-stuff-Here>
                | otherwise = return ""


showHelp :: TouchOptions -> String
showHelp opts | (displayHelp opts) = concat (intersperse "\n" helpText)
              | otherwise = ""

showVersion :: TouchOptions -> String
showVersion opts | (displayVersion opts) = concat (intersperse "\n" versionText)
                 | otherwise = ""

processArgs :: [String] -> TouchOptions -> TouchOptions
processArgs [] opts = opts
processArgs (x:xs) opts = case x of
  "--help" -> processArgs xs opts{displayHelp = True}
  "--version" -> processArgs xs opts{displayVersion = True}
  _ -> processArgs xs opts

stripQuotes :: String -> String
stripQuotes ('"':xs) = if last xs == '"' then init xs else ('"':xs)
stripQuotes xs = xs





parseFlags :: [String] -> Parser TouchOptions
parseFlags (x:xs) = return defaultTouch
parseFlags [] = return defaultTouch



data CommandFlags = CF { commandDescription :: String
                                 , shortTag :: Char
                                 , longTag :: String
                                 , effectParams :: Integer
                                 , effect :: (TouchOptions -> TouchOptions)
                                 }
instance Show CommandFlags where
  show (CF desc '\0' long _params _effect) = "--"++ long ++"  "++ desc
  show (CF desc short "" _params  _effect) = "-"++(short:[]) ++"  "++ desc
  show (CF desc short long _params _effect) = "-"++(short:[]) ++" --"++ long ++"  "++ desc

touchFlags :: [CommandFlags]
touchFlags = 
  [ (CF "display this help and exit"
        '\0' "help" 0
        (\x -> x{displayHelp = True}))
  , (CF "output version information and exit"
        '\0' "version" 0
        (\x -> x{displayVersion = True}))
  , (CF "-                     change only the access time"
        'a' "" 0
        (\x -> x{targetTypes = (targetTypes x)++[AccessTime]}))
  , (CF "-c, --        do not create any files"
        '\0' "no-create" 0
        (\x -> x{doNotCreate = True}))
  , (CF "-, --=STRING      parse STRING and use it instead of current time"
        'd' "date" 1
        (\x -> x{targetString = ""}))
  , (CF "-                     (ignored)"
        'f' "" 0
        (\x -> x{displayVersion = True}))
  , (CF "-, --   affect each symbolic link instead of any referenced file (useful only on systems that can change the timestamps of a symlink)"
        'h' "no-dereference" 0
        (\x -> x{targetDereference = True}))
  , (CF "-                     change only the modification time"
        'm' "" 0
        (\x -> x{targetTypes = targetTypes x++[ModificationTime]}))
  , (CF "-, --=FILE   use this file's times instead of current time"
        'r' "reference" 0
        (\x -> x{targetTimeFile = ""}))
  , (CF "- STAMP               use [[CC]YY]MMDDhhmm[.ss] instead of current time"
        't' "" 0
        (\x -> x{targetStamp = ""}))
  , (CF "--=WORD        change the specified time:"
        '\0' "time" 0
        (\x -> x{targetTypes = []}))
  ]

defaultTouch :: TouchOptions
defaultTouch = TouchOptions False False [] [] False False [] [] []

data TimeTypes = AccessTime | ModificationTime deriving (Eq, Show)

data TouchOptions = TouchOptions
  { displayHelp :: Bool
  , displayVersion :: Bool
  , targets :: [String]
--  , accessTimeOnly :: Bool
--  , modTimeOnly :: Bool
  , targetTypes :: [TimeTypes]
  , targetDereference :: Bool
  , doNotCreate :: Bool
  , targetTimeFile :: String
  , targetStamp :: String
  , targetString :: String
  } deriving (Show, Eq)

helpText :: [String]
helpText = [ "Update the access and modification times of each FILE to the current time."
           , "A FILE argument that does not exist is created empty, unless -c or -h is supplied."
           , "A FILE argument string of - is handled specially and causes touch to"
           , "change the times of the file associated with standard output."
           , "Mandatory arguments to long options are mandatory for short options too."
           , "  "
           , "  "
           , "  "
           , "  "
           , "  "
           , "  "
           , "  "
           , "  "
           , "      "
           , "                           WORD is access, atime, or use: equivalent to -a"
           , "                           WORD is modify or mtime: equivalent to -m"
           , "      --help     display this help and exit"
           , "      --version  output version information and exit"
           , "Note that the -d and -t options accept different time-date formats."
           , "GNU coreutils online help: <http://www.gnu.org/software/coreutils/>"
           , "Full documentation at: <http://www.gnu.org/software/coreutils/touch>"
           , "or available locally via: info '(coreutils) touch invocation'\n"
           ]

versionText :: [String]
versionText = [ "Htouch (Haskell implementation of GNU touch) 1.0"
              , "derrived from: touch (GNU coreutils) 8.23.147-35217"
              , "Copyright (C) 2015 Free Software Foundation, Inc."
              , "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
              , "This is free software: you are free to change and redistribute it."
              , "There is NO WARRANTY, to the extent permitted by law."
              , "Written by Paul Rubin, Arnold Robbins, Jim Kingdon,"
              , "David MacKenzie, and Randy Smith."
              , "Ported by PuZZleDucK.\n"
              ]

