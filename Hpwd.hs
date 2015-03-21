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

  runTermOutput term (termText (showHelp defaultDefault))
  runTermOutput term (termText (showVersion defaultDefault))
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



defaultPwd :: PwdOptions
defaultPwd = PwdOptions True
defaultDefault :: DefaultOptions
defaultDefault = DefaultOptions False False []

--newtype DefaultOptions = PwdOptions
--  help :: a
--  others :: a

data PwdOptions = PwdOptions
  { --displayHelp :: Bool
  --, displayVersion :: Bool
   resolveSymlinks :: Bool
  --, targets :: [String]
  } deriving (Show, Eq)


data GnuOption = GO (Either DefaultOptions PwdOptions)

genericizeDefault :: DefaultOptions -> GnuOption
genericizeDefault (DefaultOptions b1 b2 lst) = GO (Left (DefaultOptions b1 b2 lst))

genericizeCustom :: PwdOptions -> GnuOption
genericizeCustom (PwdOptions b1) = GO (Right (PwdOptions b1))

allFlags :: [OptionFlags GnuOption]
allFlags = [ 
    (OF "" '\0' "" (\x -> case x of
    GO (Left opts) -> GO (Left opts{targets = (targets opts)})
  ))
  , (OF "" '\0' "" (\x -> case x of
    GO (Right opts) -> GO (Right opts{resolveSymlinks = False})
  ))
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

