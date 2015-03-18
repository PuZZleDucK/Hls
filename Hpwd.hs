-- Hpwd, a haskell implementation of GNU pwd.
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Main where
import System.Environment
import System.Console.Terminfo.Base
import Data.List
import GnUtils

main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs
  let options = processArgs pwdFlags args defaultPwd

--  runTermOutput term (termText (showHelp options))
--  runTermOutput term (termText (showVersion options))
  output <- showOutput options
  runTermOutput term (termText (output))
  runTermOutput term (termText ("Options: "++(stripQuotes (show options))++"\n"))
  return ()

showOutput :: PwdOptions -> IO String
showOutput opts  = return "" -- <do-stuff-Here>
--                | otherwise = return ""


showHelp :: DefaultOptions -> String
showHelp opts | (displayHelp opts) = concat (intersperse "\n" helpText)
              | otherwise = ""

showVersion :: DefaultOptions -> String
showVersion opts | (displayVersion opts) = concat (intersperse "\n" versionText)
                 | otherwise = ""

--data ProcessingState = Normal | LongOpt | ShortOpt | NoOpts









--processArgs (x:xs) opts = case x of
--  "--help" -> processArgs xs opts{displayHelp = True}
--  "--version" -> processArgs xs opts{displayVersion = True}
--  "--" -> opts --GNU extension, terminate option parsing, still need to parse targets
--  _ -> processArgs xs opts -- if POSIXLY_CORRECT: processTargets xs opts
--                           -- as POSIX terminates option processing after the first target
--processArgs (x:xs) opts = if thisOption == "" then processArgs xs opts -- no targets
--                                              else (effect (getOptionFlag x)) opts
--  where thisOption = getOption x

--getOption :: String -> String -- "" means target/not-option
--getOption [] = ""
--getOption (x:[]) = ""
--getOption (x:y:[]) = ""
--getOption str = if head str /= '-' then ""
--                                   else if head ((drop 1) str) == '-' then (drop 2) str
--                                                                      else (drop 1) str

--getOptionFlag :: String -> OptionFlags
--getOptionFlag (ch:[]) = head (filter (\x -> (shortTag x == ch)) pwdFlags)
--getOptionFlag str = head (filter (\x -> (longTag x == str)) pwdFlags)


defaultPwd :: PwdOptions
defaultPwd = PwdOptions True


--newtype DefaultOptions = PwdOptions
--  help :: a
--  others :: a

data PwdOptions = PwdOptions
  { --displayHelp :: Bool
  --, displayVersion :: Bool
   resolveSymlinks :: Bool
  --, targets :: [String]
  } deriving (Show, Eq)



defaultFlags :: [OptionFlags DefaultOptions]
defaultFlags = 
  [ (OF ""
        '\0' ""
        (\x -> x{targets = (targets x)}))
  , (OF "display this help and exit"
        '\0' "help"
        (\x -> x{displayHelp = True}))
  , (OF "output version information and exit"
        '\0' "version"
        (\x -> x{displayVersion = True}))
  ]

pwdFlags :: [OptionFlags PwdOptions]
pwdFlags = 
  [ (OF "use PWD from environment, even if it contains symlinks"
        'L' "logical"
        (\x -> x{resolveSymlinks = False}))
  , (OF "avoid all symlinks"
        'P' "physical"
        (\x -> x{resolveSymlinks = True}))
  ]

helpText :: [String]
helpText = [ "Usage: /home/bminerds/x/coreutils/src/pwd [OPTION]..."
           , "Print the full filename of the current working directory."
           , "  -L, --logical   use PWD from environment, even if it contains symlinks"
           , "  -P, --physical  avoid all symlinks"
           , "      --help     display this help and exit"
           , "      --version  output version information and exit"
           , "If no option is specified, -P is assumed."
           , "NOTE: your shell may have its own version of pwd, which usually supersedes"
           , "the version described here.  Please refer to your shell's documentation"
           , "for details about the options it supports."
           , "GNU coreutils online help: <http://www.gnu.org/software/coreutils/>"
           , "Full documentation at: <http://www.gnu.org/software/coreutils/pwd>"
           , "or available locally via: info '(coreutils) pwd invocation'\n"
           ]

versionText :: [String]
versionText = [ "H<app-name> (Haskell implementation of GNU <app-name>) 1.0"
              , "derrived from: pwd (GNU coreutils) 8.23.126-99f76"
              , "Copyright (C) 2015 Free Software Foundation, Inc."
              , "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
              , "This is free software: you are free to change and redistribute it."
              , "There is NO WARRANTY, to the extent permitted by law."
              , "Written by Jim Meyering."
              , "Ported by PuZZleDucK.\n"
              ]

