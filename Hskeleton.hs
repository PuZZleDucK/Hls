-- H<app-name>, a haskell implementation of GNU <app-name>.
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
  output <- showOutput options
  runTermOutput term (termText (output))
  return ()

showOutput :: <app-name>Options -> IO String
showOutput opts | not ((displayHelp opts) || (displayVersion opts)) = return "" -- <do-stuff-Here>
                | otherwise = return ""


showHelp :: <app-name>Options -> String
showHelp opts | (displayHelp opts) = concat (intersperse "\n" helpText)
              | otherwise = ""

showVersion :: <app-name>Options -> String
showVersion opts | (displayVersion opts) = concat (intersperse "\n" versionText)
                 | otherwise = ""

processArgs :: [String] -> <app-name>Options -> <app-name>Options
processArgs [] opts = opts
processArgs (x:xs) opts = case x of
  "--help" -> processArgs xs opts{displayHelp = True}
  "--version" -> processArgs xs opts{displayVersion = True}
  _ -> processArgs xs opts

stripQuotes :: String -> String
stripQuotes ('"':xs) = if last xs == '"' then init xs else ('"':xs)
stripQuotes xs = xs

defaultOptions :: <app-name>Options
defaultOptions = <app-name>Options False False

data <app-name>Options = <app-name>Options
  { displayHelp :: Bool
  , displayVersion :: Bool } deriving (Show, Eq)


helpText :: [String]
helpText = [ ""
           , "keep the newline->\n"
           ]

versionText :: [String]
versionText = [ "H<app-name> (Haskell implementation of GNU <app-name>) 1.0"
              , "derrived from: "
              , "Ported by PuZZleDucK.\n"
              ]

