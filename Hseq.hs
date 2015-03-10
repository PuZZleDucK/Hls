-- Hseq, a haskell implementation of GNU seq.
module Main where
import System.Environment
import System.Console.Terminfo.Base
import Data.List

main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs
  let options = processArgs args defaultSeq
--  runTermOutput term (termText ("Options: "++(show options)++"\n"))

  runTermOutput term (termText (showHelp options))
  runTermOutput term (termText (showVersion options))
  output <- showOutput options
  runTermOutput term (termText (output++"\n"))
  return ()

showOutput :: SeqOptions -> IO String
showOutput opts | not ((displayHelp opts) || (displayVersion opts)) = return (concat (intersperse sep (map show [sStart,(sStart+sInc)..sEnd])))
                | otherwise = return ""
  where sStart = if (seqStart opts) == (-1) then 1 else seqStart opts
        sEnd = if (seqEnd opts) == (-1) then 1 else seqEnd opts
        sInc = if (seqIncrement opts) == (-1) then 1 else seqIncrement opts
        sep = displaySeperator opts


showHelp :: SeqOptions -> String
showHelp opts | (displayHelp opts) = concat (intersperse "\n" helpText)
              | otherwise = ""

showVersion :: SeqOptions -> String
showVersion opts | (displayVersion opts) = concat (intersperse "\n" versionText)
                 | otherwise = ""

processArgs :: [String] -> SeqOptions -> SeqOptions
processArgs [] opts = opts
processArgs (x:xs) opts = case x of
  "--help" -> processArgs xs opts{displayHelp = True}
  "--version" -> processArgs xs opts{displayVersion = True}
  "-s" -> processArgs (drop 1 xs) opts{displaySeperator = head xs}
  z -> if (seqEnd opts) == (-1)
    then processArgs xs opts{seqEnd = (read z)::Integer} --oh, needs a bit of tweaking: [E] [S E] [S I E]
    else if (seqStart opts) == (-1)
      then processArgs xs opts{seqEnd = (read z)::Integer, seqStart = (seqEnd opts)}
      else if (seqIncrement opts) == (-1)
        then processArgs xs opts{seqEnd = (read z)::Integer, seqIncrement = (seqEnd opts)}
        else processArgs xs opts

stripQuotes :: String -> String
stripQuotes ('"':xs) = if last xs == '"' then init xs else ('"':xs)
stripQuotes xs = xs

defaultSeq :: SeqOptions
defaultSeq = SeqOptions False False (-1) (-1) (-1) "" "\n" False

data SeqOptions = SeqOptions
  { displayHelp :: Bool
  , displayVersion :: Bool
  , seqStart :: Integer
  , seqEnd :: Integer
  , seqIncrement :: Integer
  , displayFormat :: String
  , displaySeperator :: String
  , padLeadingZeros :: Bool } deriving (Show, Eq)


helpText :: [String]
helpText = [ "Usage: /home/bminerds/x/coreutils/src/seq [OPTION]... LAST"
           , "  or:  /home/bminerds/x/coreutils/src/seq [OPTION]... FIRST LAST"
           , "  or:  /home/bminerds/x/coreutils/src/seq [OPTION]... FIRST INCREMENT LAST"
           , "Print numbers from FIRST to LAST, in steps of INCREMENT."
           , "Mandatory arguments to long options are mandatory for short options too."
           , "  -f, --format=FORMAT      use printf style floating-point FORMAT"
           , "  -s, --separator=STRING   use STRING to separate numbers (default: \n)"
           , "  -w, --equal-width        equalize width by padding with leading zeroes"
           , "      --help     display this help and exit"
           , "      --version  output version information and exit"
           , "If FIRST or INCREMENT is omitted, it defaults to 1.  That is, an"
           , "omitted INCREMENT defaults to 1 even when LAST is smaller than FIRST."
           , "The sequence of numbers ends when the sum of the current number and"
           , "INCREMENT would become greater than LAST."
           , "FIRST, INCREMENT, and LAST are interpreted as floating point values."
           , "INCREMENT is usually positive if FIRST is smaller than LAST, and"
           , "INCREMENT is usually negative if FIRST is greater than LAST."
           , "FORMAT must be suitable for printing one argument of type 'double';"
           , "it defaults to %.PRECf if FIRST, INCREMENT, and LAST are all fixed point"
           , "decimal numbers with maximum precision PREC, and to %g otherwise."
           , "GNU coreutils online help: <http://www.gnu.org/software/coreutils/>"
           , "Full documentation at: <http://www.gnu.org/software/coreutils/seq>"
           , "or available locally via: info '(coreutils) seq invocation'\n"
           ]

versionText :: [String]
versionText = [ "Hseq (Haskell implementation of GNU seq) 1.0"
              , "derrived from: seq (GNU coreutils) 8.23.126-99f76"
              , "Copyright (C) 2015 Free Software Foundation, Inc."
              , "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
              , "This is free software: you are free to change and redistribute it."
              , "There is NO WARRANTY, to the extent permitted by law."
              , "Written by Ulrich Drepper."
              , "Ported by PuZZleDucK.\n"
              ]

