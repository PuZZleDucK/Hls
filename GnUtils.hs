-- GnUtils, utils for a haskell implementation of GNU core-utils.
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module GnUtils where
import System.Environment
import System.Console.Terminfo.Base
import Data.List




optionDelimiter = '-'
optionTerminator = "--"
paramaterDelimiter = "="
optionParamaterDelimiter = ":"



data OptionParamater = OP (Either () String)
data OptionToken = OT String OptionParamater | TargetToken String
--data Arguments = [OptionToken]
--data TargetToken = 

data ProgramOption a = ProgramOption {
  optionText :: String
, optionShortFlags :: [String]
, optionLongFlags :: [String]
, optionParamaters :: [String]
, optionEffect :: ConfigurationData -> ConfigurationData
, optionValue :: a
}

helpOption :: ProgramOption Bool
helpOption = ProgramOption "" [] [] [] (\x->x) True

defaultOptions :: [ProgramOption Bool]
defaultOptions = [ (ProgramOption "" [] ["help"] [] (\x->x) False)
                 , (ProgramOption "" [] ["version"] [] (\x->x) False)
                 ]

data ConfigurationData = ConfigurationData {
  boolData :: [ProgramOption Bool]
, stringData :: [ProgramOption String]
, integerData :: [ProgramOption Integer]
, floatData :: [ProgramOption Float]
}

data ProgramData = ProgramData {
  appName :: String
, appHelp :: String
, appVersion :: String
, argumentStrings :: [String]
--, argumentTokens :: [OptionToken]
, configuration :: ConfigurationData
, longParser :: ConfigurationData -> String -> ConfigurationData
, shortParser :: ConfigurationData -> String -> ConfigurationData
}


parseArguments :: ProgramData -> [String] -> ProgramData
parseArguments dat [] = dat
parseArguments dat (arg:args) = parseArguments dat args


parseArgument :: ProgramData -> String -> ProgramData
parseArgument dat [] = dat
parseArgument dat (marker1:marker2:rest) = if marker1 == optionDelimiter
  then if marker2 == optionDelimiter
    then parseLongOption dat rest
    else parseShortOption dat (marker2:rest)
  else dat  -- {argumentTokens = (argumentTokens dat)++[TargetToken (marker1:marker2:rest)]}

--check for --help --version and -- here
parseLongOption :: ProgramData -> String -> ProgramData
parseLongOption dat [] = dat
--parseLongOption dat "help" = dat{configuration=((configuration dat){boolData=(boolData (configuration dat))++[]})}
parseLongOption dat "help" = addOption dat True helpOption
parseLongOption dat long = dat{configuration=(configParser long)}
  where oldConfig = configuration dat
        configParser = (longParser dat) oldConfig

parseShortOption :: ProgramData -> String -> ProgramData
parseShortOption dat [] = dat
parseShortOption dat shorts = dat{configuration=(configParser shorts)}
  where oldConfig = configuration dat
        configParser = (shortParser dat) oldConfig



addOption :: ProgramData -> a -> ProgramOption a -> ProgramData
addOption dat x opt = dat{configuration = thisEffect (configuration dat)}
  where thisEffect = optionEffect opt



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


--allGnuFlags :: [OptionFlags GnuOption]
--allGnuFlags = [ 
--    (OF "" '\0' "" (\x -> case x of
--    GO (Left opts) -> GO (Left opts{targets = (targets opts)})
--  ))
--  , (OF "" '\0' "" (\x -> case x of
--    GO (Right opts) -> GO (Right opts{resolveSymlinks = False})
--  ))
--  ]




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









