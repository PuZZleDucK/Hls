-- GnUtils, utils for a haskell implementation of GNU core-utils.
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module GnUtils where
import System.Environment
import System.Console.Terminfo.Base
import Data.List




doHelp :: ProgramData -> Bool
doHelp (ProgramData nam hlp ver args (ConfigurationData b s i f) lpars spars) = optionValue thisOpt -- True -- thisOpt
  where thisOpt = (head (fst (partition (\x -> "h" `elem` optionShortFlags x) b)))

getHelp :: ProgramData -> String
getHelp (ProgramData nam hlp ver args cfg lpars spars) = fst hlp ++ "<options go here>" ++ snd hlp

doVersion :: ProgramData -> Bool
doVersion (ProgramData nam hlp ver args (ConfigurationData b s i f) lpars spars) = optionValue thisOpt -- True -- thisOpt
  where thisOpt = (head (fst (partition (\x -> "v" `elem` optionShortFlags x) b)))

getVersion :: ProgramData -> String
getVersion (ProgramData nam hlp ver args cfg lpars spars) = ver



optionDelimiter = '-'
optionTerminator = "--"
paramaterDelimiter = "="
optionParamaterDelimiter = ":"



data OptionParamater = OP (Either () String) deriving (Eq, Show)
data OptionToken = OT String OptionParamater | TargetToken String deriving (Eq, Show)
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

instance Show a => Show (ProgramOption a) where
  show (ProgramOption txt shrt lng param eff val) = 
    "{"++(show lng)
    ++ (show shrt) ++ "_"++(show param)++"_}==>" ++ (show val)



getBool :: ConfigurationData -> String -> ProgramOption Bool
getBool opts key = if length key > 1
  then getLongBool opts key
  else getShortBool opts key

getLongBool :: ConfigurationData -> String -> ProgramOption Bool
getLongBool cfg str = head (filter (\x -> str `elem` (optionShortFlags x)) (boolData cfg))

getShortBool :: ConfigurationData -> String -> ProgramOption Bool
getShortBool cfg str = head (filter (\x -> str `elem` (optionShortFlags x)) (boolData cfg))

getString :: ConfigurationData -> String -> ProgramOption String
getString opts key = if length key > 1
  then getLongString opts key
  else getShortString opts key

getLongString :: ConfigurationData -> String -> ProgramOption String
getLongString cfg str = head (filter (\x -> str `elem` (optionShortFlags x)) (stringData cfg))

getShortString :: ConfigurationData -> String -> ProgramOption String
getShortString cfg str = head (filter (\x -> str `elem` (optionShortFlags x)) (stringData cfg))

getInteger :: ConfigurationData -> String -> ProgramOption Integer
getInteger opts key = if length key > 1
  then getLongInteger opts key
  else getShortInteger opts key

getLongInteger :: ConfigurationData -> String -> ProgramOption Integer
getLongInteger cfg str = head (filter (\x -> str `elem` (optionShortFlags x)) (integerData cfg))

getShortInteger :: ConfigurationData -> String -> ProgramOption Integer
getShortInteger cfg str = head (filter (\x -> str `elem` (optionShortFlags x)) (integerData cfg))

getFloat :: ConfigurationData -> String -> ProgramOption Float
getFloat opts key = if length key > 1
  then getLongFloat opts key
  else getShortFloat opts key

getLongFloat :: ConfigurationData -> String -> ProgramOption Float
getLongFloat cfg str = head (filter (\x -> str `elem` (optionShortFlags x)) (floatData cfg))

getShortFloat :: ConfigurationData -> String -> ProgramOption Float
getShortFloat cfg str = head (filter (\x -> str `elem` (optionShortFlags x)) (floatData cfg))



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
} deriving (Show)

data ProgramData = ProgramData {
  appName :: String
, appHelp :: (String,String)
, appVersion :: String
, argumentStrings :: [String]
--, argumentTokens :: [OptionToken]
, configuration :: ConfigurationData
, longParser :: ConfigurationData -> String -> ConfigurationData
, shortParser :: ConfigurationData -> String -> ConfigurationData
}

instance Show (ProgramData) where
  show (ProgramData nam hlp ver args cfg lpars spars) =
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

--check for --help --version and -- here
parseLongOption :: ProgramData -> String -> ProgramData
parseLongOption dat [] = dat
--parseLongOption dat "help" = dat{configuration=((configuration dat){boolData=(boolData (configuration dat))++[]})}
parseLongOption dat "help" = addOption dat True helpOption
parseLongOption dat "version" = addOption dat True versionOption
parseLongOption dat long = dat{configuration=(configParser long)}
  where configParser = (longParser dat) (configuration dat)
        

parseShortOption :: ProgramData -> String -> ProgramData
parseShortOption dat [] = dat
parseShortOption dat "h" = addOption dat True helpOption
parseShortOption dat "v" = addOption dat True versionOption
parseShortOption dat shorts = dat{configuration=(configParser shorts)}
  where configParser = (shortParser dat) (configuration dat)
        



addOption :: ProgramData -> a -> ProgramOption a -> ProgramData
addOption dat x opt = dat{configuration = thisEffect (configuration dat)}
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
processShorts flags ((c:rst):others) opts | c == '-' = processLong flags (rst:others) opts
                                          | otherwise = processArgs flags others (processShortOptions flags (c:rst) opts)

processLong :: [OptionFlags x] -> [String] -> x -> x
processLong flags (opt:others) opts = processArgs flags others ((getLongEffect flags opt) opts)



processArgs :: [OptionFlags x] -> [String] -> x -> x
processArgs flags [] opts = opts
--processArgs NoOpts remaining opts = opts{targets=(targets opts) ++ remaining}
processArgs flags ((c:rst):others) opts | c == '-' = processShorts flags (rst:others) opts
                                        | otherwise = processArgs flags others opts --targets is not visible here!!! {targets = (targets opts)++[(c:rst)]}









