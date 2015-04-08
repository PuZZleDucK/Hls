-- GUtils, utils for a haskell implementation of GNU core-utils.

module GUtils where
import Data.List

optionDelimiter :: Char
optionDelimiter = '-'
_optionTerminator :: String
_optionTerminator = "--"
_paramaterDelimiter :: String
_paramaterDelimiter = "="
_optionParamaterDelimiter :: String
_optionParamaterDelimiter = ":"

data OptValue = BoolOpt Bool -- now I can pattern match on option sub-types
              | StringOpt String 
              | IntOpt Integer 
              | FloatOpt Float
              | IntRangeOpt Integer Integer

data ProgramOpt = ProgramOpt {
  optText :: String
, optShortFlags :: [String]
, optLongFlags :: [String]
, optParamaters :: [String]
, optEffect :: ConfigurationData -> ConfigurationData
, optValue :: OptValue -- match me
}

data ProgramOption a = ProgramOption {
  optionText :: String
, optionShortFlags :: [String]
, optionLongFlags :: [String]
, optionParamaters :: [String]
, optionEffect :: ConfigurationData -> ConfigurationData
, optionValue :: a
}

instance Show a => Show (ProgramOption a) where
  show (ProgramOption _txt shrt lng param _eff val) = 
    "\n{"++(show lng)
    ++ (show shrt) ++ "_"++(show param)++"_}==>{" ++ (show val) ++ "}"




setOption :: [ProgramOption a] -> String -> a -> [ProgramOption a]
setOption opts key value = if length key > 1
  then setLongOption opts key value
  else setShortOption opts key value

setLongOption :: [ProgramOption a] -> String -> a -> [ProgramOption a]
setLongOption opts key value = (head thisOpt){optionValue = value}:otherOpts
  where (thisOpt,otherOpts) = partition (\x -> key `elem` optionLongFlags x) opts

setShortOption :: [ProgramOption a] -> String -> a -> [ProgramOption a]
setShortOption opts key value = (head thisOpt){optionValue = value}:otherOpts
  where (thisOpt,otherOpts) = partition (\x -> key `elem` optionShortFlags x) opts


helpOption :: ProgramOption Bool
helpOption = ProgramOption "Help text"
                           ["h"]
                           ["help"]
                           [] 
                           (\x->x{boolData = setOption (boolData x) "h" True})
                           False

versionOption :: ProgramOption Bool
versionOption = ProgramOption "Version text"
                              ["v"]
                              ["version"]
                              [] 
                              (\x->x{boolData = setOption (boolData x) "v" True})
                              False

defaultOptions :: [ProgramOption Bool]
defaultOptions = [ helpOption
                 , versionOption
                 ]

data ConfigurationData = ConfigurationData {
  boolData :: [ProgramOption Bool]
, stringData :: [ProgramOption String]
, integerData :: [ProgramOption Integer]
, floatData :: [ProgramOption Float]
}

instance Show (ConfigurationData) where
  show (ConfigurationData b s i f) =
    "\nBool:"++(show b)++
    "\nString:"++(show s)++
    "\nInt:"++(show i)++
    "\nFloat"++(show f)


data ProgramData = ProgramData {
  appName :: String
, appHelp :: (String,String)
, appVersion :: String
, argumentStrings :: [String]
, configuration :: ConfigurationData
, parser :: ProgramData -> String -> ProgramData
, oParser :: ProgramData -> String -> String -> ProgramData
}

instance Show (ProgramData) where
  show (ProgramData nam _hlp _ver _args cfg _lpars _loparse _spars) =
    " :: " ++ nam ++ " :: "++(show cfg)



parseArguments :: ProgramData -> [String] -> ProgramData
parseArguments dat [] = dat
parseArguments dat (arg:args) = parseArguments (newDat) args
  where newDat = parseArgument dat arg


parseArgument :: ProgramData -> String -> ProgramData
parseArgument dat [] = dat
parseArgument dat (marker1:marker2:rest) = if marker1 == optionDelimiter
  then if marker2 == optionDelimiter
    then parseLongOption dat rest
    else parseShortOption dat (marker2:rest)
  else dat  -- {argumentTokens = (argumentTokens dat)++[TargetToken (marker1:marker2:rest)]}
parseArgument dat _ = dat

--check for --help --version and -- here
parseLongOption :: ProgramData -> String -> ProgramData
parseLongOption dat [] = dat
--parseLongOption dat "help" = dat{configuration=((configuration dat){boolData=(boolData (configuration dat))++[]})}
parseLongOption dat "help" = addOption dat True helpOption
parseLongOption dat "version" = addOption dat True versionOption
parseLongOption dat long = configParser long
  where configParser = (longParser dat) dat


parseShortOption :: ProgramData -> String -> ProgramData
parseShortOption dat [] = dat
parseShortOption dat "h" = addOption dat True helpOption
parseShortOption dat "v" = addOption dat True versionOption
parseShortOption dat shorts = configParser shorts
  where configParser = (shortParser dat) dat
        



addOption :: ProgramData -> a -> ProgramOption a -> ProgramData
addOption dat _x opt = dat{configuration = thisEffect (configuration dat)}
  where thisEffect = optionEffect opt




-- old and crusty from here on... to prune out of programs



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
processShorts flags ((chr:rst):others) opts 
  | chr == '-' = processLong flags (rst:others) opts
  | otherwise = processArgs flags others (processShortOptions flags (chr:rst) opts)
processShorts _ _ opts = opts

processLong :: [OptionFlags x] -> [String] -> x -> x
processLong flags (opt:others) opts = processArgs flags others ((getLongEffect flags opt) opts)
processLong _ _ opts = opts



processArgs :: [OptionFlags x] -> [String] -> x -> x
processArgs _flags [] opts = opts
--processArgs NoOpts remaining opts = opts{targets=(targets opts) ++ remaining}
processArgs flags ((c:rst):others) opts | c == '-' = processShorts flags (rst:others) opts
                                        | otherwise = processArgs flags others opts --targets is not visible here!!! {targets = (targets opts)++[(c:rst)]}
processArgs _ _ opts = opts










