-- Hecho, a haskell implementation of GNU echo.
module Main where
import System.Environment
import System.Console.Terminfo.Base
import Data.List
import Text.Parsec hiding ((<|>), many, try)
import Text.ParserCombinators.Parsec
--import Text.Parsec.String
--import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Char()
import Numeric

main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs
  let options = processArgs args defaultEcho
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
processText (c:cx) = if isEscape c then (escape cx):(processText (drop 1 cx))
                                   else c:(processText cx)

escape :: [Char] -> Char
escape text | head text == '\\' = '\\'
            | head text == 'a' = '\a'     -- alert (BEL)"
            | head text == 'b' = '\b'     -- backspace"
--escape 'c' = '\'      produce no further output"
--escape 'e' = '\'      escape"
            | head text == 'f' = '\f'     -- form feed"
            | head text == 'n' = '\n'
            | head text == 'r' = '\r'     -- carriage return"
            | head text == 't' = '\t'     -- horizontal tab"
            | head text == 'v' = '\v'     -- vertical tab"
--might be better to use parsec at this point
--0NNN   byte with octal value NNN (1 to 3 digits)"
--xHH    byte with hexadecimal value HH (1 to 2 digits)"
escape _ = '#'


-- usage: parse parseHex [] "x41stuff"
parseHex :: Parser Char
parseHex = do _ <- char 'x'
              a <- hexDigit
              b <- hexDigit
--              let ((d,_):_) = readHex [a,b]
              let d = fst (head (readHex [a,b]))
              return . toEnum $ d

parseOctal :: CharParser () Char
parseOctal = do _ <- char '0'
                a <- octDigit
                b <- octDigit
                c <- octDigit
--                let ((d,_):_) = readOct [a,b,c]
                let d = fst (head (readOct [a,b,c]))
                return . toEnum $ d

parseShortMarker :: Parser ()
parseShortMarker = do _ <- char '-'
                      return ()

parseLongMarker :: Parser ()
parseLongMarker = do _ <- char '-'
                     _ <- char '-'
                     return ()

--parseMarker :: Parser ()
--parseMarker = do try parseLongMarker <|> 
--                 try parseShortMarker <|>
--                 parseTarget


parseShortOption :: EchoOptions -> Parser EchoOptions
parseShortOption opts = do parseShortMarker
                           next <- anyChar
                           case next of
                             'n' -> return opts{suppressNewline = True}
                             'e' -> return opts{enableEscapeSequences = True}
                             'E' -> return opts{enableEscapeSequences = False}
                             _ -> return opts

parseThisShortOption :: EchoOptions -> Char -> Parser EchoOptions
parseThisShortOption opts ch = return ( opts{effect = (head (filter (\x -> (shortTag x)==ch) echoFlags))}) 

--parseThisLongOption :: EchoOptions -> String -> EchoOptions
--parseThisLongOption opts str = (effect (head (filter (\x -> (longTag x)==str) echoFlags))) opts

parseTargetOption :: EchoOptions -> String -> Parser EchoOptions
parseTargetOption opts str = return (opts{echoText = (echoText opts)++[str]})


--data CommandFlags = CF { commandDescription :: String
--                                 , shortTag :: Char
--                                 , longTag :: String
--                                 , effect :: (EchoOptions -> EchoOptions)
--                                 }




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
  z -> processArgs xs opts{echoText = (echoText opts)++[z]}

stripQuotes :: String -> String
stripQuotes ('"':xs) = if last xs == '"' then init xs else ('"':xs)
stripQuotes xs = xs

defaultEcho :: EchoOptions
defaultEcho = EchoOptions False False False False []

data EchoOptions = EchoOptions
  { displayHelp :: Bool
  , displayVersion :: Bool
  , suppressNewline :: Bool
  , enableEscapeSequences :: Bool
  , echoText :: [String] } deriving (Show, Eq)

data CommandFlags = CF { commandDescription :: String
                                 , shortTag :: Char
                                 , longTag :: String
                                 , effect :: (EchoOptions -> EchoOptions)
                                 }
instance Show CommandFlags where
  show (CF desc '\0' long _effect) = "--"++ long ++"  "++ desc
  show (CF desc short "" _effect) = "-"++(short:[]) ++"  "++ desc
  show (CF desc short long _effect) = "-"++(short:[]) ++" --"++ long ++"  "++ desc

echoFlags :: [CommandFlags]
echoFlags = [ (CF "do not output the trailing newline"
                  'n' ""
                  (\x -> x{suppressNewline = True}))
--                  parseTargetOption)
            , (CF "enable interpretation of backslash escapes"
                  'e' ""
--                  (\x -> x{enableEscapeSequences = True}))
                  undefined)
            , (CF "disable interpretation of backslash escapes (default)"
                  'E' ""
--                  (\x -> x{enableEscapeSequences = False}))
                  undefined)
            , (CF "display this help and exit"
                  '\0' "help"
--                  (\x -> x{displayHelp = True}))
                  undefined)
            , (CF "output version information and exit"
                  '\0' "version"
--                  (\x -> x{displayVersion = True}))
                  undefined)
            ]

helpText :: [String]
helpText = [ "Usage: ./src/echo [SHORT-OPTION]... [STRING]..."
           , "  or:  ./src/echo LONG-OPTION"
           , "Echo the STRING(s) to standard output."
           , foldr (\x y -> ("    "++(show x)++"\n") ++ y) "" echoFlags
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

