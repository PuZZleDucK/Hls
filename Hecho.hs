-- Hecho, a haskell implementation of GNU echo.
module Main where
import System.Environment
import System.Console.Terminfo.Base
import Data.List
import Control.Monad

main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs
  let options = processArgs args defaultOptions
--  runTermOutput term (termText ("Options: "++(show options)++"\n"))

  runTermOutput term (termText (showHelp options))
  runTermOutput term (termText (showVersion options))
  
  runTermOutput term (termText (showOutput options))
  return ()

showOutput :: EchoOptions -> String
showOutput opts | not ((displayHelp opts) || (displayVersion opts)) = (concat (intersperse " " displayText))++seperator
                | otherwise = ""
  where seperator = if (suppressNewline opts) then "" else "\n"
        displayText = if (enableEscapeSequences opts) then processEscapes (echoText opts)
                                                      else echoText opts

processEscapes :: [String] -> [String]
processEscapes [] = []
processEscapes (x:xs) = (processText x):(processEscapes xs)

processText :: String -> String
processText [] = []
processText (c:cx) = if isEscape c then (escape (head cx)):(processText (drop 1 cx))
                                   else c:(processText cx)

escape :: Char -> Char
escape _ = '#'

isEscape :: Char -> Bool
isEscape c = if c == '\\' then True else False

showHelp :: EchoOptions -> String
showHelp opts | (displayHelp opts) = concat (intersperse "\n" helpText)
              | otherwise = ""

showVersion :: EchoOptions -> String
showVersion opts | (displayVersion opts) = concat (intersperse "\n" versionText)
                 | otherwise = ""

processArgs :: [String] -> EchoOptions -> EchoOptions
processArgs [] opts = opts
processArgs (x:xs) opts = case x of
  "--help" -> processArgs xs opts{displayHelp = True}
  "--version" -> processArgs xs opts{displayVersion = True}
  "-n" -> processArgs xs opts{suppressNewline = True}
  "-e" -> processArgs xs opts{enableEscapeSequences = True}
  "-E" -> processArgs xs opts{enableEscapeSequences = False}
  x -> processArgs xs opts{echoText = (echoText opts)++[x]}

stripQuotes :: String -> String
stripQuotes ('"':xs) = if last xs == '"' then init xs else ('"':xs)
stripQuotes xs = xs

defaultOptions :: EchoOptions
defaultOptions = EchoOptions False False False False []

data EchoOptions = EchoOptions
  { displayHelp :: Bool
  , displayVersion :: Bool
  , suppressNewline :: Bool
  , enableEscapeSequences :: Bool
  , echoText :: [String] } deriving (Show, Eq)


helpText :: [String]
helpText = [ "Usage: ./src/echo [SHORT-OPTION]... [STRING]..."
           , "  or:  ./src/echo LONG-OPTION"
           , "Echo the STRING(s) to standard output."
           , "  -n do not output the trailing newline"
           , "  -e             enable interpretation of backslash escapes"
           , "  -E             disable interpretation of backslash escapes (default)"
           , "      --help display this help and exit"
           , "      --version output version information and exit"
           , "If -e is in effect, the following sequences are recognised:"
           , "  \\\\      backslash"
           , "  \\a      alert (BEL)"
           , "  \\b      backspace"
           , "  \\c      produce no further output"
           , "  \\e      escape"
           , "  \\f      form feed"
           , "  \\n      new line"
           , "  \\r      carriage return"
           , "  \\t      horizontal tab"
           , "  \\v      vertical tab"
           , "  \\0NNN   byte with octal value NNN (1 to 3 digits)"
           , "  \\xHH    byte with hexadecimal value HH (1 to 2 digits)"
           , "NOTE: your shell may have its own version of echo, which usually supersedes"
           , "the version described here. Please refer to your shell's documentation"
           , "for details about the options it supports."
           , "GNU coreutils online help: <http://www.gnu.org/software/coreutils/>"
           , "Full documentation at: <http://www.gnu.org/software/coreutils/echo>"
           , "or available locally via: info '(coreutils) echo invocation'\n"
           ]

versionText :: [String]
versionText = [ "H<app-name> (Haskell implementation of GNU <app-name>) 1.0"
              , "derrived from: echo (GNU coreutils) 8.23.138-7ceaf"
              , "Copyright (C) 2015 Free Software Foundation, Inc."
              , "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
              , "This is free software: you are free to change and redistribute it."
              , "There is NO WARRANTY, to the extent permitted by law."
              , "Written by Brian Fox and Chet Ramey."
              , "Ported by PuZZleDucK.\n"
              ]

