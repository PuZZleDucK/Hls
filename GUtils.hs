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

data OptionValue = BoolOpt Bool
                 | StringOpt String
                 | ListOpt [String]
                 | IntOpt Integer 
                 | FloatOpt Float
                 | IntRangeOpt Integer Integer deriving Show

data OptionEffect = OptionEffect (Options -> String -> [String] -> (Options,[String]))
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

targetOption :: Option
targetOption = Option "Targets"
                      (Flags [""] [""])
                      (ListOpt [])
                      (OptionEffect (\opts newTarget unused -> (appendFlag opts "" ((StringOpt newTarget)),unused) ))

helpOption :: Option
helpOption = Option "Help text"
                    (Flags ["h"] ["help"])
                    (BoolOpt False)
                    (OptionEffect (\(opts) _ unused -> (replaceFlag opts "help" (BoolOpt True),unused)))

versionOption :: Option
versionOption = Option "Version text"
                       (Flags ["v"] ["version"])
                       (BoolOpt False)
                       (OptionEffect (\(opts) _ unused -> (replaceFlag opts "version" (BoolOpt True),unused)))

errorOption :: Option
errorOption = Option "<<ERROR>>" (Flags [] []) (BoolOpt False) (OptionEffect (\x _ u -> (x,u)))

defaultOptions :: Options
defaultOptions = Options [ helpOption
                         , versionOption
                         , targetOption
                         ]

catOptions  :: Options -> Options -> Options
catOptions (Options opts1) (Options opts2) = Options (opts1++opts2)

replaceFlag :: Options -> String -> OptionValue -> Options
replaceFlag opts str value = addFlag (setValue (getFlag (getFlagOrPrefix str) opts) value) (removeFlag (getFlagOrPrefix str) opts)

appendFlag :: Options -> String -> OptionValue -> Options
appendFlag opts str value = addFlag (appendValue (getFlag (getFlagOrPrefix str) opts) value) (removeFlag (getFlagOrPrefix str) opts)

setValue :: Option -> OptionValue -> Option
setValue opt val = opt{value = val}

appendValue :: Option -> OptionValue -> Option
appendValue opt@(Option _ _ (ListOpt vals) _) (StringOpt val) = opt{value = ListOpt (vals++[val])}

addFlag :: Option -> Options -> Options
addFlag opt (Options opts) = Options (opt:opts)

removeFlag :: String -> Options -> Options
removeFlag str (Options opts) = Options (filter (\x -> not (isFlag (getFlagOrPrefix str) x)) opts)

safeHead :: a -> [a] -> a
safeHead def [] = def
safeHead _ (x:_) = x

getFlag :: String -> Options -> Option
getFlag str (Options opts) = safeHead errorOption (filter (\x -> (isFlag (str) x)) opts)

isFlag :: String -> Option -> Bool -- not handling single val params -x=<val>
isFlag str option = if elem (getFlagOrPrefix str) (long (flags option))
  then True
  else elem (getFlagOrPrefix str) (short (flags option))

getFlagOrPrefix :: String -> String
getFlagOrPrefix str | elem '=' str = ((takeWhile (\x -> x /= '=') str))
                    | otherwise = str


setTrue :: Option -> Option
setTrue option = option{ value = BoolOpt True }

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
parseArguments dat (arg:args) = parseArguments (newDat) unusedArgs
  where (newDat,unusedArgs) = parseArgument dat arg args
-- ...also need to return unused-args... should have used parsec :p

--need to add param for 'rest-of-args' incase needed by sub-options...
parseArgument :: ProgramData -> String -> [String] -> (ProgramData,[String])
parseArgument dat (marker1:marker2:rest) unused = if marker1 == optionDelimiter
  then if marker2 == optionDelimiter
    then (parseLongOption dat rest unused)
    else (parseShortOption dat (marker2:rest) unused)
  else (addTarget dat (marker1:marker2:rest) unused)--targets here
parseArgument dat _ unused = (dat,unused)

addTarget :: ProgramData -> String -> [String] -> (ProgramData,[String])
addTarget dat target unused  = (dat{configuration = newCfg}, stillUnused)
  where cfg = configuration dat
        (OptionEffect effect) = paramaterEffect (getFlag "" cfg)
        (newCfg, stillUnused) = (effect cfg target unused)

parseLongOption :: ProgramData -> String -> [String] -> (ProgramData,[String])
parseLongOption dat [] unused = (dat,unused)
parseLongOption dat str unused = (dat{configuration = newCfg},stillUnused)
  where cfg = configuration dat
        (OptionEffect effect) = paramaterEffect (getFlag str cfg)
        (newCfg, stillUnused) = (effect cfg (str) unused)

parseShortOption :: ProgramData -> String -> [String] -> (ProgramData,[String])
parseShortOption dat [] unused = (dat,unused)
parseShortOption dat str unused = (dat{configuration = newCfg}, stillUnused)
  where cfg = configuration dat
        (OptionEffect effect) = paramaterEffect (getFlag (str) cfg)
        (newCfg, stillUnused) = (effect cfg (str) unused)






parseOptionFileName :: [String] -> (String,[String])
parseOptionFileName (x1:x2:xs) | length x1 == 1 = (x2,xs)--single letter flag
  | elem '=' x1 = (drop 1 (dropWhile (\x -> x /= '=') x1),(x2:xs)) -- one word -flag=<value>
  | otherwise = (x2,xs)--long flag with space




