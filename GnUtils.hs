-- GnUtils, utils for a haskell implementation of GNU core-utils.
module GnUtils where
import System.Environment
import System.Console.Terminfo.Base
import Data.List


data DefaultOptions = DefaultOptions
  { displayHelp :: Bool
  , displayVersion :: Bool
  , targets :: [String]
  } deriving (Show, Eq)


stripQuotes :: String -> String
stripQuotes ('"':xs) = if last xs == '"' then init xs else ('"':xs)
stripQuotes xs = xs


flagPrefixes :: [String]
flagPrefixes = ["-","--","+"]
flagChars :: [Char]
flagChars = ['-','+']



data OptionFlags x = OF { description :: String
                         , shortTag :: Char
                         , longTag :: String
                         , effect :: x -> x --options modifier
                         }

instance Show (OptionFlags x) where
  show (OF desc '\0' long _effect) = "--"++ long ++"  "++ desc
  show (OF desc short "" _effect) = "-"++(short:[]) ++"  "++ desc
  show (OF desc short long _effect) = "-"++(short:[]) ++" --"++ long ++"  "++ desc


getShortEffect :: [OptionFlags x] -> Char -> (x -> x)
getShortEffect flags ch = effect (head (filter (\x -> shortTag x == ch) flags))

getLongEffect :: [OptionFlags x] -> String -> (x -> x)
getLongEffect flags str = effect (head (filter (\x -> longTag x == str) flags))

processShortOptions :: [OptionFlags x] -> String -> x -> x
processShortOptions _ [] opts = opts
processShortOptions flags (x:xs) opts = processShortOptions flags (xs) ((getShortEffect flags x) opts)

processShorts :: [OptionFlags x] -> [String] -> x -> x
processShorts flags ((c:rst):others) opts | c == '-' = processLong flags (rst:others) opts
                                          | otherwise = processArgs flags others (processShortOptions flags (c:rst) opts)

processLong :: [OptionFlags x] -> [String] -> x -> x
processLong flags (opt:others) opts = processArgs flags others ((getLongEffect flags opt) opts)



processArgs :: [OptionFlags x] -> [String] -> x -> x
processArgs flags [] opts = opts
--processArgs NoOpts remaining opts = opts{targets=(targets opts) ++ remaining}
processArgs flags ((c:rst):others) opts | c == '-' = processShorts flags (rst:others) opts
                                        | otherwise = processArgs flags others opts --targets is not visible here!!! {targets = (targets opts)++[(c:rst)]}









