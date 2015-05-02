-- GUtils, utils for a haskell implementation of GNU core-utils.

module GUtils where
import Data.List
--import System.Console.Terminfo.Base
import System.Posix

optionDelimiter :: Char
optionDelimiter = '-'
optionTerminator :: String
optionTerminator = "--"
paramaterDelimiter :: Char
paramaterDelimiter = '='
_optionParamaterDelimiter :: String
_optionParamaterDelimiter = ":"

data OptionValue = BoolOpt Bool
                 | StringOpt String
                 | ListOpt [String]
                 | IntOpt Integer
                 | FloatOpt Float
                 | GnuSizeOpt GnuSize
                 | GnuRangeOpt GnuRange deriving Show

--SUFFIX may be 's' for seconds (the default),
--'m' for minutes, 'h' for hours or 'd' for days
data TimeSuffix = Seconds | Minutes | Hours | Days | NoTimeSuffix
instance Show TimeSuffix where
  show Seconds = "s"
  show Hours = "h"
  show Minutes = "m"
  show Days = "d"
  show NoTimeSuffix = ""
instance Read TimeSuffix where
  readsPrec _ ('s':xs) = [(Seconds,xs)]
  readsPrec _ ('h':xs) = [(Hours,xs)]
  readsPrec _ ('m':xs) = [(Minutes,xs)]
  readsPrec _ ('d':xs) = [(Days,xs)]
  readsPrec _ xs = [(NoTimeSuffix,xs)]

data GnuTime = GnuTime { units::Float, unitType::TimeSuffix }
instance Show GnuTime where
  show (GnuTime unit suffix) = (show unit)++(show suffix)
instance Read GnuTime where
  readsPrec _ (x) = do (num,xx) <- readsPrec 0 x :: [(Float,String)]
                       (suff,xxxx) <- readsPrec 0 xx :: [(TimeSuffix,String)]
                       [((GnuTime num suff),xxxx)]

secondsInMinutes, secondsInHours, secondsInDays :: Float
secondsInMinutes = 60
secondsInHours = 60*secondsInMinutes
secondsInDays = 24*secondsInHours

timeToFloat :: GnuTime -> Float
timeToFloat (GnuTime unit Seconds) = unit
timeToFloat (GnuTime unit Minutes) = unit
timeToFloat (GnuTime unit Hours) = unit * secondsInHours
timeToFloat (GnuTime unit Days) = unit
timeToFloat (GnuTime unit NoTimeSuffix) = unit

data GnuRange = GnuRange { min::Integer, max::Integer } deriving Show

data GnuSize = GnuSize Prefix Integer Units
data Prefix = NoPrefix | Extend | Reduce | AtMost | AtLeast | RoundDown | RoundUp
data Units = Kilo UnitType
           | Mega UnitType
           | Giga UnitType
           | Tera UnitType
           | Peta UnitType
           | Eta UnitType
           | Zeta UnitType
           | Yota UnitType
           | NoUnits
data UnitType = I024 | Bytes -- 1024 | 1000

instance Show Prefix where
  show NoPrefix = ""
  show Extend = "+"
  show Reduce = "-"
  show AtMost = "<"
  show AtLeast = ">"
  show RoundDown = "/"
  show RoundUp = "%"
instance Show UnitType where
  show I024 = ""
  show Bytes = "B"
instance Show Units where
  show (Kilo x) = "K"++(show x)
  show (Mega x) = "M"++(show x)
  show (Giga x) = "G"++(show x)
  show (Tera x) = "T"++(show x)
  show (Peta x) = "P"++(show x)
  show (Eta x) = "E"++(show x)
  show (Zeta x) = "Z"++(show x)
  show (Yota x) = "Y"++(show x)
  show (NoUnits) = ""
instance Show GnuSize where
  show (GnuSize pre num unit) = (show pre)++(show num)++(show unit)

instance Read Prefix where
  readsPrec _ ('+':xs) = [(Extend,xs)]
  readsPrec _ ('-':xs) = [(Reduce,xs)]
  readsPrec _ ('<':xs) = [(AtMost,xs)]
  readsPrec _ ('>':xs) = [(AtLeast,xs)]
  readsPrec _ ('/':xs) = [(RoundDown,xs)]
  readsPrec _ ('%':xs) = [(RoundUp,xs)]
  readsPrec _ (xs) = [(NoPrefix,xs)]
instance Read Units where
  readsPrec _ ("K") = [(Kilo I024,"")]
  readsPrec _ ('K':u:xs) | u == 'B'= [(Kilo Bytes,"")]
                         | otherwise = [(Kilo I024,(u:xs))]
  readsPrec _ ("M") = [(Mega I024,"")]
  readsPrec _ ('M':u:xs) | u == 'B'= [(Mega Bytes,"")]
                         | otherwise = [(Mega I024,(u:xs))]
  readsPrec _ ("G") = [(Giga I024,"")]
  readsPrec _ ('G':u:xs) | u == 'B'= [(Giga Bytes,"")]
                         | otherwise = [(Giga I024,(u:xs))]
  readsPrec _ ("T") = [(Tera I024,"")]
  readsPrec _ ('T':u:xs) | u == 'B'= [(Tera Bytes,"")]
                         | otherwise = [(Tera I024,(u:xs))]
  readsPrec _ ("P") = [(Peta I024,"")]
  readsPrec _ ('P':u:xs) | u == 'B'= [(Peta Bytes,"")]
                         | otherwise = [(Peta I024,(u:xs))]
  readsPrec _ ("E") = [(Eta I024,"")] --ezy
  readsPrec _ ('E':u:xs) | u == 'B'= [(Eta Bytes,"")]
                         | otherwise = [(Eta I024,(u:xs))]
  readsPrec _ ("Z") = [(Zeta I024,"")]
  readsPrec _ ('Z':u:xs) | u == 'B'= [(Zeta Bytes,"")]
                         | otherwise = [(Zeta I024,(u:xs))]
  readsPrec _ ("Y") = [(Yota I024,"")]
  readsPrec _ ('Y':u:xs) | u == 'B'= [(Yota Bytes,"")]
                         | otherwise = [(Yota I024,(u:xs))]
  readsPrec _ xs = [(NoUnits,xs)]

instance Read GnuSize where
  readsPrec _ (x) = do (pre,xx) <- readsPrec 0 x :: [(Prefix,String)]
                       (num,xxx) <- readsPrec 0 xx :: [(Integer,String)]
                       (unit,xxxx) <- readsPrec 0 xxx :: [(Units,String)]
                       [((GnuSize pre num unit),xxxx)]
    
calcGnuSize :: GnuSize -> Integer
calcGnuSize size = case size of
  (GnuSize _ n NoUnits) -> n
  (GnuSize _ n (Kilo Bytes)) -> n*(10^(3::Integer))
  (GnuSize _ n (Kilo I024)) -> n*(2^(10::Integer))
  (GnuSize _ n (Mega Bytes)) -> n*(10^(6::Integer))
  (GnuSize _ n (Mega I024)) -> n*(2^(20::Integer))
  (GnuSize _ n (Giga Bytes)) -> n*(10^(9::Integer))
  (GnuSize _ n (Giga I024)) -> n*(2^(30::Integer))
  (GnuSize _ n (Tera Bytes)) -> n*(10^(12::Integer))
  (GnuSize _ n (Tera I024)) -> n*(2^(40::Integer))
  (GnuSize _ n (Peta Bytes)) -> n*(10^(15::Integer))
  (GnuSize _ n (Peta I024)) -> n*(2^(50::Integer))
  (GnuSize _ n (Eta Bytes)) -> n*(10^(18::Integer))
  (GnuSize _ n (Eta I024)) -> n*(2^(60::Integer))
  (GnuSize _ n (Zeta Bytes)) -> n*(10^(21::Integer))
  (GnuSize _ n (Zeta I024)) -> n*(2^(70::Integer))
  (GnuSize _ n (Yota Bytes)) -> n*(10^(24::Integer))
  (GnuSize _ n (Yota I024)) -> n*(2^(80::Integer))

data OptionEffect = OptionEffect (Options -> String -> [String] -> (Options,[String]))
instance Show (OptionEffect) where
  show (OptionEffect _effect) = "(\\x -> z)"

data Flags = Flags { short :: [String], long :: [String] } deriving Show

data Option = Option {
  helpText :: String
, flags :: Flags -- double pun?
, flagParam :: String
, value :: OptionValue
, paramaterEffect :: OptionEffect
}
instance Show Option where
  show (Option _txt (Flags sFlg lFlg) _par val _eff) = (show lFlg)++(show sFlg)++"\t ==>  "++(show val)

data Options = Options [Option]
instance Show (Options) where
  show (Options opts) = '\n':(concat (intersperse "\n" (map show opts)))

targetOption :: Option
targetOption = Option "Targets"
                      (Flags [""] [optionTerminator]) ""
                      (ListOpt [])
                      (OptionEffect (\opts newTarget unused -> (appendFlag opts "--"  ((StringOpt newTarget)),unused) ))

helpOption :: Option
helpOption = Option "Help text"
                    (Flags [""] ["help"]) ""
                    (BoolOpt False)
                    (OptionEffect (\(opts) _ unused -> (replaceFlag opts "help" (BoolOpt True),unused)))

versionOption :: Option
versionOption = Option "Version text"
                       (Flags [""] ["version"]) ""
                       (BoolOpt False)
                       (OptionEffect (\(opts) _ unused -> (replaceFlag opts "version" (BoolOpt True),unused)))

errorOption :: Option
errorOption = Option "<<ERROR>>" (Flags [] []) "" (BoolOpt False) (OptionEffect (\x _ u -> (x,u)))

defaultOptions :: Options
defaultOptions = Options [ helpOption
                         , versionOption
                         , targetOption ]

catOptions  :: Options -> Options -> Options
catOptions (Options opts1) (Options opts2) = Options (opts1++opts2)

replaceFlag :: Options -> String -> OptionValue -> Options
replaceFlag opts str val = addFlag (setValue (getFlag (getFlagOrPrefix str) opts) val) (removeFlag (getFlagOrPrefix str) opts)

appendFlag :: Options -> String -> OptionValue -> Options
appendFlag opts str val = addFlag (appendValue (getFlag (getFlagOrPrefix str) opts) val) (removeFlag (getFlagOrPrefix str) opts)

setValue :: Option -> OptionValue -> Option
setValue opt val = opt{value = val}

getList :: Option -> [String]
getList opt = case value opt of
  (ListOpt x) -> x
  _ -> []
getBool :: Option -> Bool
getBool opt = case value opt of
  (BoolOpt x) -> x
  _ -> False
getString :: Option -> String
getString opt = case value opt of
  (StringOpt x) -> x
  _ -> ""
getInt :: Option -> Integer
getInt opt = case value opt of
  (IntOpt x) -> x
  _ -> 0
getFloat :: Option -> Float
getFloat opt = case value opt of
  (FloatOpt x) -> x
  _ -> 0.0
getSize :: Option -> GnuSize
getSize opt = case value opt of
  (GnuSizeOpt x) -> x
  _ -> GnuSize NoPrefix 0 NoUnits
getRange :: Option -> GnuRange
getRange opt = case value opt of
  (GnuRangeOpt x) -> x
  _ -> GnuRange 0 0

appendValue :: Option -> OptionValue -> Option
appendValue opt@(Option _ _ _ (ListOpt vals) _) (StringOpt val) = opt{value = ListOpt (vals++[val])}
appendValue opt _ = opt

addFlag :: Option -> Options -> Options
addFlag opt (Options opts) = Options (opt:opts)

removeFlag :: String -> Options -> Options
removeFlag str (Options opts) = Options (filter (\x -> not (isFlag (getFlagOrPrefix str) x)) opts)

safeHead :: a -> [a] -> a
safeHead def [] = def
safeHead _ (x:_) = x

getFlag :: String -> Options -> Option
getFlag str (Options opts) = safeHead errorOption (filter (\x -> (isFlag (str) x)) opts)

isFlag :: String -> Option -> Bool
isFlag "" _ = False
isFlag str option = if elem (getFlagOrPrefix str) (long (flags option))
  then True
  else elem (getFlagOrPrefix str) (short (flags option))

getFlagOrPrefix :: String -> String
getFlagOrPrefix str | elem '=' str = ((takeWhile (          /= '=') str))
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
parseArguments dat ("--":rest) = addAllTargets dat rest
parseArguments dat (arg:args) = parseArguments (newDat) unusedArgs
  where (newDat,unusedArgs) = parseArgument dat arg args

parseArgument :: ProgramData -> String -> [String] -> (ProgramData,[String])
parseArgument dat (marker1:marker2:rest) unused = if marker1 == optionDelimiter
  then if marker2 == optionDelimiter
    then (parseLongOption dat rest unused)
    else (parseShortOption dat (marker2:rest) unused)
  else (addTarget dat (marker1:marker2:rest) unused)
parseArgument dat param unused = (addTarget dat param unused)

addAllTargets :: ProgramData -> [String] -> ProgramData
addAllTargets dat (target:rest)  = addAllTargets finalCfg rest
  where newCfg = (addTarget dat target rest)
        (finalCfg,_) = newCfg
addAllTargets dat []  = dat

addTarget :: ProgramData -> String -> [String] -> (ProgramData,[String])
addTarget dat target unused  = (dat{configuration = newCfg}, stillUnused)
  where cfg = configuration dat
        (OptionEffect effect) = paramaterEffect (getFlag optionTerminator cfg)
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
parseOptionFileName (x1:x2:xs) | length x1 == 1 = (x2,xs)--single letter
  | elem '=' x1 = (drop 1 (dropWhile (/= paramaterDelimiter) x1),(x2:xs)) -- -flag=<value>
  | otherwise = (x2,xs)--long flag with space
parseOptionFileName x = ("",x)

parseOptionSize :: [String] -> (GnuSize,[String])
parseOptionSize (x) = (size,(tail trimmedText))
  where trimmedText = trimLeading x
        parseResults = readsPrec 0 (head trimmedText)
        (size,_) = head parseResults

trimLeading :: [String] -> [String]
trimLeading (x1:xs) | length x1 == 1 = ([head xs]++tail xs)
                    | elem '=' x1 = [drop 1 (dropWhile (/= paramaterDelimiter) x1)]++(xs)
                    | otherwise = xs
trimLeading [] = []

showHelp :: ProgramData -> String
showHelp dat = case value (getFlag "help" (configuration dat)) of
  (BoolOpt doHelp) -> if doHelp 
                         then preText++optionText++"\n"++postText
                         else ""
  _ -> ""
  where preText = fst (appHelp dat)
        optionText = showConfigHelps (configuration dat)
        postText = snd (appHelp dat)

showConfigHelps :: Options -> String
showConfigHelps (Options opts) = concat (map showConfigHelp opts)

showConfigHelp :: Option -> String
showConfigHelp opt | longTag /= optionTerminator = tagText++helpString
                   | otherwise = "" --target option hidden
  where longTag = head (long $ flags opt)
        longText = " --"++longTag
        shortTag = head (short $ flags opt)
        shortText = if length shortTag == 1
                       then "\n  -"++shortTag++","
                       else "\n     "
        helpString = helpText opt
        tagText = padString (shortText++longText++paramText) " " 25
        paramText = flagParam opt

padString :: String -> String -> Integer -> String
padString str pad count | (length str) >= (fromInteger count) = str
                        | otherwise = padString (str++pad) pad count

showVersion :: ProgramData -> String
showVersion dat = case value (getFlag "version" (configuration dat)) of
  (BoolOpt doVersion) -> if doVersion
                            then appVersion dat
                            else ""
  _ -> ""









-- -----------------------------------------------------------------------
  -- File utils... new module?
-- -----------------------------------------------------------------------

--blocksize: cat /proc/mounts
--               and search for blksize=
--or better yet (for block IO): stat --format=%o <target>
getFileSize :: String -> IO Integer
getFileSize str = do
  stat <- getFileStatus str
  let (COff size) = fileSize stat
  putStrLn (">>>>"++str++"{"++(show size)++"}") --dbg
  return (fromIntegral size)




