-- Lets try something a little less ambitious, like basename.
module Main where
import System.Environment
import System.Console.Terminfo.Base
import Data.List
import System.FilePath
import Data.Text (stripSuffix, pack, dropWhile, splitOn, dropAround) -- pack to make text
import Data.Maybe
import Control.Monad

main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs
  let options = processArgs args defaultOptions

  runTermOutput term (termText (showError options))
  runTermOutput term (termText (showHelp options))
  runTermOutput term (termText (showVersion options))
  runTermOutput term (termText ((showOutput options)++"\n"))
--  runTermOutput term (termText ("Options: "++(show options)++"\n"))
  return ()

showOutput :: BasenameOptions -> String
showOutput opts | doOutput = concat (intersperse interChar (formatOutput (targets opts) opts))
                | otherwise = ""
  where interChar = if (suppressNewline opts) then "" else "\n"
        doOutput = not ((displayHelp opts) || (displayVersion opts))


-- Data.List.last (Data.Text.splitOn (pack (pathSeparator:[])) (pack "this/that/other"))
formatOutput :: [String] -> BasenameOptions -> [String]
formatOutput [] _ = []
formatOutput (x:xs) opts =  ((stripQuotes) finalText):(formatOutput xs opts)
  where suffix =  (removeSuffix opts)
        seperator = pack (pathSeparator:[])
        splitText = stripQuotes (show (last (splitOn seperator (pack x))))
        finalText = (stripSuffixIfThere (suffix) splitText)


stripSuffixIfThere :: String -> String -> String
stripSuffixIfThere suff str = case (stripSuffix (pack suff) (pack str)) of Nothing -> str
                                                                           Just x -> show (dropAround (\x -> x=='"') (x))

showError :: BasenameOptions -> String
showError opts | (null (targets opts)) = concat (intersperse "\n" errorTest)
               | otherwise = ""

showHelp :: BasenameOptions -> String
showHelp opts | (displayHelp opts) = concat (intersperse "\n" helpText)
              | otherwise = ""

showVersion :: BasenameOptions -> String
showVersion opts | (displayVersion opts) = concat (intersperse "\n" versionText)
                 | otherwise = ""

processArgs :: [String] -> BasenameOptions -> BasenameOptions
processArgs [] opts = opts
processArgs (x:xs) opts = case x of
  "--help" -> processArgs xs opts{displayHelp = True}
  "--version" -> processArgs xs opts{displayVersion = True}
  "--zero" -> processArgs xs opts{suppressNewline = True}
  "-z" -> processArgs xs opts{suppressNewline = True}
  "--multiple" -> processArgs xs opts{multipleInputs = True}
  "-a" -> processArgs xs opts{multipleInputs = True}
  "-s" -> processArgs (drop 1 xs) opts{removeSuffix = head xs}
  _ -> if (x `startsWith` "--suffix=") then processArgs xs opts{removeSuffix = (drop 9 x)}
                                       else processArgs xs opts{targets = (targets opts)++[x]}

startsWith :: String -> String -> Bool
startsWith (x:xs) (y:ys) | x==y = startsWith xs ys
                         | otherwise = False
startsWith _ [] = True
startsWith [] _ = False

stripQuotes :: String -> String
stripQuotes ('"':xs) = if last xs == '"' then init xs else ('"':xs)
stripQuotes xs = xs

defaultOptions :: BasenameOptions
defaultOptions = BasenameOptions [] False False "" False False

data BasenameOptions = BasenameOptions { targets :: [String]
                                       , multipleInputs :: Bool
                                       , suppressNewline :: Bool --zero
                                       , removeSuffix :: String
                                       , displayVersion :: Bool
                                       , displayHelp :: Bool } deriving (Show, Eq)


helpText :: [String]
helpText = [ "Usage: basename NAME [SUFFIX]"
           , "  or:  basename OPTION... NAME..."
           , "Print NAME with any leading directory components removed."
           , "If specified, also remove a trailing SUFFIX."
           , "Mandatory arguments to long options are mandatory for short options too."
           , "  -a, --multiple       support multiple arguments and treat each as a NAME"
           , "  -s, --suffix=SUFFIX  remove a trailing SUFFIX; implies -a"
           , "  -z, --zero           end each output line with NUL, not newline"
           , "      --help display this help and exit"
           , "      --version output version information and exit"
           , "Examples:"
           , "  basename /usr/bin/sort          -> \"sort\""
           , "  basename include/stdio.h .h     -> \"stdio\""
           , "  basename -s .h include/stdio.h  -> \"stdio\""
           , "  basename -a any/str1 any/str2   -> \"str1\" followed by \"str2\""
           , "GNU coreutils online help: <http://www.gnu.org/software/coreutils/>\n"
           ]

versionText :: [String]
versionText = [ "Hbasename (Haskell implementation of GNU basename) 1.0"
              , "derrived from: basename (GNU coreutils) 8.23"
              , "Copyright (C) 2014 Free Software Foundation, Inc."
              , "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
              , "This is free software: you are free to change and redistribute it."
              , "There is NO WARRANTY, to the extent permitted by law."
              , "Written by David MacKenzie."
              , "Ported by PuZZleDucK.\n"
              ]

errorTest = [ "basename: missing operand"
            , "Try `basename --help' for more information.\n"
            ]

