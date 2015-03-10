-- Hhead, a haskell implementation of GNU head.
module Main where
import System.Environment
import System.Console.Terminfo.Base
import Data.List
import System.IO

main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs
  let options = processArgs args defaultHead
--  runTermOutput term (termText ("Options: "++(show options)++"\n"))

  runTermOutput term (termText (showHelp options))
  runTermOutput term (termText (showVersion options))
  output <- showOutput options
  runTermOutput term (termText (output))
  return ()

showOutput :: HeadOptions -> IO String
showOutput opts | not ((displayHelp opts) || (displayVersion opts)) = do
  handles <- sequence (map ((flip openFile) ReadMode) ((targetFiles opts)))
  contents <- sequence (map (hGetLines lineCount) (handles))
  return ((concat (intersperse "\n" (map concat (contents)))++"\n"))
                | otherwise = return ""
  where lineCount = fromInteger (if (countLines opts) == 0 then 10 else (countLines opts))
--lineCount is screwey for some reason

hGetLines :: Integer -> Handle -> IO [String] --single line only.
hGetLines _count file = do contents <- hGetLine file
                           return [contents]

showHelp :: HeadOptions -> String
showHelp opts | (displayHelp opts) = concat (intersperse "\n" helpText)
              | otherwise = ""

showVersion :: HeadOptions -> String
showVersion opts | (displayVersion opts) = concat (intersperse "\n" versionText)
                 | otherwise = ""

processArgs :: [String] -> HeadOptions -> HeadOptions
processArgs [] opts = opts
processArgs (x:xs) opts = case x of
  "--help" -> processArgs xs opts{displayHelp = True}
  "--version" -> processArgs xs opts{displayVersion = True}
  "-n" -> processArgs (drop 1 xs) opts{countLines = (read (head xs))::Integer}
  "-c" -> processArgs (drop 1 xs) opts{countBytes = (read (head xs))::Integer}
  "-q" -> processArgs xs opts{suppressHeaders = True}
  "-v" -> processArgs xs opts{displayHeaders = True}
  z -> processArgs xs opts{targetFiles = (targetFiles opts++[z])}

stripQuotes :: String -> String
stripQuotes ('"':xs) = if last xs == '"' then init xs else ('"':xs)
stripQuotes xs = xs

defaultHead :: HeadOptions
defaultHead = HeadOptions False False [] 0 0 False False

data HeadOptions = HeadOptions
  { displayHelp :: Bool
  , displayVersion :: Bool
  , targetFiles :: [String]
  , countBytes :: Integer-- Will need better parsing for this with KB kb MB etc
  , countLines :: Integer
  , suppressHeaders :: Bool
  , displayHeaders :: Bool } deriving (Show, Eq)


helpText :: [String]
helpText = [ "Usage: /home/bminerds/x/coreutils/src/head [OPTION]... [FILE]..."
           , "Print the first 10 lines of each FILE to standard output."
           , "With more than one FILE, precede each with a header giving the file name."
           , "With no FILE, or when FILE is -, read standard input."
           , "Mandatory arguments to long options are mandatory for short options too."
           , "  -c, --bytes=[-]K         print the first K bytes of each file;"
           , "                             with the leading '-', print all but the last"
           , "                             K bytes of each file"
           , "  -n, --lines=[-]K         print the first K lines instead of the first 10;"
           , "                             with the leading '-', print all but the last"
           , "                             K lines of each file"
           , "  -q, --quiet, --silent    never print headers giving file names"
           , "  -v, --verbose            always print headers giving file names"
           , "      --help     display this help and exit"
           , "      --version  output version information and exit"
           , "K may have a multiplier suffix:"
           , "b 512, kB 1000, K 1024, MB 1000*1000, M 1024*1024,"
           , "GB 1000*1000*1000, G 1024*1024*1024, and so on for T, P, E, Z, Y."
           , "GNU coreutils online help: <http://www.gnu.org/software/coreutils/>"
           , "Full documentation at: <http://www.gnu.org/software/coreutils/head>"
           , "or available locally via: info '(coreutils) head invocation'\n"
           ]

versionText :: [String]
versionText = [ "Hhead (Haskell implementation of GNU head) 1.0"
              , "derrived from: head (GNU coreutils) 8.23.126-99f76"
              , "Copyright (C) 2015 Free Software Foundation, Inc."
              , "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
              , "This is free software: you are free to change and redistribute it."
              , "There is NO WARRANTY, to the extent permitted by law."
              , "Written by David MacKenzie and Jim Meyering."
              , "Ported by PuZZleDucK.\n"
              ]

