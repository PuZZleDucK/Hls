-- GUtils, utils for a haskell implementation of GNU core-utils.

module GUtils where
import Data.List
import System.Console.Terminfo.Base

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
                 | GnuSizeOpt GnuSize
                 | IntRangeOpt Integer Integer deriving Show

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
  show (GnuSize pre num units) = (show pre)++(show num)++(show units)

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
    
data OptionEffect = OptionEffect (Options -> String -> [String] -> (Options,[String]))
instance Show (OptionEffect) where
  show (OptionEffect _effect) = "(\\x -> x)"

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

isFlag :: String -> Option -> Bool
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
parseArguments dat ("--":rest) = addAllTargets dat rest -- targets option
parseArguments dat (arg:args) = parseArguments (newDat) unusedArgs
  where (newDat,unusedArgs) = parseArgument dat arg args
-- ...also need to return unused-args... should have used parsec :p

--need to add param for 'rest-of-args' incase needed by sub-options...
parseArgument :: ProgramData -> String -> [String] -> (ProgramData,[String])
parseArgument dat (marker1:marker2:rest) unused = if marker1 == optionDelimiter
  then if marker2 == optionDelimiter
    then (parseLongOption dat rest unused)
    else (parseShortOption dat (marker2:rest) unused)
  else (addTarget dat (marker1:marker2:rest) unused)
parseArgument dat _ unused = (dat,unused)

addAllTargets :: ProgramData -> [String] -> ProgramData
addAllTargets dat (target:rest)  = addAllTargets finalCfg rest
  where newCfg = (addTarget dat target rest)
        (finalCfg,_) = newCfg
addAllTargets dat []  = dat

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
parseOptionFileName x = ("",x)

parseOptionSize :: [String] -> (GnuSize,[String])
parseOptionSize (x) = (size,(tail trimmedText))
  where trimmedText = trimLeading x
        parseResults = readsPrec 0 (head trimmedText) :: [(GnuSize,String)]
        (size,_) = head parseResults

trimLeading :: [String] -> [String]
trimLeading (x1:xs) | length x1 == 1 = ([head xs]++tail xs)
                    | elem '=' x1 = [drop 1 (dropWhile (\x -> x /= '=') x1)]++(xs)
                    | otherwise = xs
trimLeading [] = []

showHelp :: ProgramData -> TermOutput
showHelp dat | doHelp = termText (preText++optionText++postText)
             | otherwise = termText ""
  where (BoolOpt doHelp) = value (getFlag "help" (configuration dat))
        preText = fst (appHelp dat)
        optionText = show (configuration dat)
        postText = snd (appHelp dat)
        

showVersion :: ProgramData -> TermOutput
showVersion dat | doVer = termText (appVersion dat)
                | otherwise = termText ""
  where (BoolOpt doVer) = value (getFlag "version" (configuration dat))






