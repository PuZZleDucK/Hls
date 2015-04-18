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

data OptionValue = BoolOpt Bool -- now I can pattern match on option sub-types
                 | StringOpt String
                 | ListOpt [String]
                 | IntOpt Integer 
                 | FloatOpt Float
                 | IntRangeOpt Integer Integer deriving Show

data OptionEffect = OptionEffect (Options -> String -> Options)
instance Show (OptionEffect) where
  show (OptionEffect effect) = "(\\x -> x)"

data Flags = Flags { short :: [String], long :: [String] } deriving Show

data Option = Option {
  helpText :: String
, flags :: Flags -- double pun?
, value :: OptionValue
, paramaterEffect :: OptionEffect
} deriving Show

data Options = Options [Option]
instance Show (Options) where
  show (Options opts) = '\n':(concat (intersperse "\n" (map show opts)))

--instance Show (Option) where
--  show (Option text flags value effect) = "\n{"
--    ++(show flags) ++ "_"++(show effect)++"_}==>{" ++ (show value) ++ "}"

--setOption :: Options -> String -> String -> Options
--setOption option flag paramaters = if (length flag) > 1
--  then setLongOption option flag paramaters
--  else setShortOption option flag paramaters

--setLongOption :: Options -> String -> String -> Options
--setLongOption = undefined

--setShortOption :: Options -> String -> String -> Options
--setShortOption = undefined

--addTarget :: Options -> String -> Options
--addTarget opts target = undefined

--getOption :: String -> Options -> Option
--getOption "" (Options opts) = head (filter (\x -> elem "" (long (flags x))) opts)

targetOption :: Option
targetOption = Option "Targets"
                      (Flags [""] [""])
                      (ListOpt [])
                      (OptionEffect (\opts newTarget -> appendFlag opts "" (StringOpt newTarget)))

helpOption :: Option
helpOption = Option "Help text"
                    (Flags ["h"] ["help"])
                    (BoolOpt False)
                    (OptionEffect (\(opts) _ -> replaceFlag opts "help" (BoolOpt True)))

versionOption :: Option
versionOption = Option "Version text"
                       (Flags ["v"] ["version"])
                       (BoolOpt False)
                       (OptionEffect (\(opts) _ -> replaceFlag opts "version" (BoolOpt True)))

defaultOptions :: Options
defaultOptions = Options [ helpOption
                         , versionOption
                         , targetOption
                         ]

replaceFlag :: Options -> String -> OptionValue -> Options
replaceFlag opts str value = addFlag (setValue (getFlag str opts) value) (removeFlag str opts)

appendFlag :: Options -> String -> OptionValue -> Options
appendFlag opts str value = addFlag (appendValue (getFlag str opts) value) (removeFlag str opts)

setValue :: Option -> OptionValue -> Option
setValue opt val = opt{value = val}

appendValue :: Option -> OptionValue -> Option
appendValue opt@(Option _ _ (ListOpt vals) _) (StringOpt val) = opt{value = ListOpt (vals++[val])}

addFlag :: Option -> Options -> Options
addFlag opt (Options opts) = Options (opt:opts)

removeFlag :: String -> Options -> Options
removeFlag str (Options opts) = Options (filter (\x -> not (isFlag str x)) opts)

getFlag :: String -> Options -> Option
getFlag str (Options opts) = head (filter (\x -> (isFlag str x)) opts)

isFlag :: String -> Option -> Bool
isFlag str option = if elem str (long (flags option))
  then True
  else elem str (short (flags option))

setTrue :: Option -> Option
setTrue option = option{ value = BoolOpt True }

--instance Show (Options) where
--  show (x:xs) = "O>" ++ (show x) ++ "\n" ++ (show xs)
--  show [] = "\n"

data ProgramData = ProgramData {
  appName :: String
, appHelp :: (String,String)
, appVersion :: String
, argumentStrings :: [String]
, configuration :: Options
, parser :: Options -> String -> Options
}

instance Show (ProgramData) where
  show (ProgramData nam _hlp _ver _args cfg _parse) =
    " :: " ++ nam ++ " :: "++(show cfg)

parseArguments :: ProgramData -> [String] -> ProgramData
parseArguments dat [] = dat
parseArguments dat ("--":rest) = dat -- add rest to targets option
parseArguments dat (arg:args) = parseArguments (newDat) args
  where newDat = parseArgument dat arg

parseArgument :: ProgramData -> String -> ProgramData
--parseArgument dat [] = dat
parseArgument dat (marker1:marker2:rest) = if marker1 == optionDelimiter
  then if marker2 == optionDelimiter
    then parseLongOption dat rest
    else parseShortOption dat (marker2:rest)
  else addTarget dat (marker1:marker2:rest)--targets here
parseArgument dat _ = dat

addTarget :: ProgramData -> String -> ProgramData
addTarget dat target = dat{configuration = effect cfg target}
  where cfg = configuration dat
        (OptionEffect effect) = paramaterEffect (getFlag "" cfg)

--check for --help --version and -- here
parseLongOption :: ProgramData -> String -> ProgramData
parseLongOption dat [] = dat
parseLongOption dat str = dat{configuration = effect cfg str}
  where cfg = configuration dat
        (OptionEffect effect) = paramaterEffect (getFlag str cfg)

--parseLongOption dat "help" = dat{configuration = effect cfg ""}
--  where cfg = configuration dat
--        (OptionEffect effect) = paramaterEffect helpOption
--parseLongOption dat "help" = addOption dat True helpOption
--parseLongOption dat "version" = addOption dat True versionOption
--parseLongOption dat long = configParser long
--  where configParser = (longParser dat) dat


parseShortOption :: ProgramData -> String -> ProgramData
parseShortOption dat [] = dat
parseShortOption dat str = dat{configuration = effect cfg str}
  where cfg = configuration dat
        (OptionEffect effect) = paramaterEffect (getFlag str cfg)
--parseShortOption dat "h" = addOption dat True helpOption
--parseShortOption dat "v" = addOption dat True versionOption
--parseShortOption dat shorts = configParser shorts
--  where configParser = (shortParser dat) dat
        



--addOption :: ProgramData -> a -> ProgramOption a -> ProgramData
--addOption dat _x opt = dat{configuration = thisEffect (configuration dat)}
--  where thisEffect = optionEffect opt












