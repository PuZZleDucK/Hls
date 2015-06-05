-- -----------------------------------------------------------------------
-- GOptions, utils for manipulating GNU options for haskell version of GNU core-utils.

module GOptions where
import Data.List

optionDelimiter :: Char
optionDelimiter = '-'
optionTerminator :: String
optionTerminator = "--"
paramaterDelimiter :: Char
paramaterDelimiter = '='
_optionParamaterDelimiter :: String
_optionParamaterDelimiter = ":"

data Option = Option {
  helpText :: String
, flags :: Flags -- double pun?
, flagParam :: String
, value :: OptionValue
, paramaterEffect :: OptionEffect
}
instance Show Option where
  show (Option _txt (Flags sFlg lFlg) _par val _eff) =
    (show lFlg)++(show sFlg)++"\t ==>  "++(show val)

data Options = Options [Option]
instance Show (Options) where
  show (Options opts) = '\n':(concat (intersperse "\n" (map show opts)))

data OptionValue = BoolOpt Bool
                 | StringOpt String
                 | ListOpt [String]
                 | IntOpt Integer
                 | FloatOpt Float
                 | GnuSizeOpt GnuSize
                 | GnuRangeOpt GnuRange deriving Show

data OptionEffect = OptionEffect (Options -> String -> [String] -> (Options,[String]))
instance Show (OptionEffect) where
  show (OptionEffect _effect) = "(\\x -> z)"

data Flags = Flags { short :: [String], long :: [String] } deriving Show

appendFlag :: Options -> String -> OptionValue -> Options
appendFlag opts str val = addFlag (appendValue jFlag val) (removeFlag (getFlagOrPrefix str) opts)
  where jFlag = case (getFlag (getFlagOrPrefix str) opts) of
                 (Just f) -> f
                 Nothing -> (Option "<<ERROR>>" (Flags [] []) "" (BoolOpt False) (OptionEffect (\x _ u -> (x,u))))

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

getTargets :: Options -> [String] --oh no, what have I done
getTargets opts = case (getFlag "--" opts) of
                    (Just targets) -> case value targets of
                                 (ListOpt targetList) -> targetList
                                 _ -> []
                    _ -> []

appendValue :: Option -> OptionValue -> Option
appendValue opt@(Option _ _ _ (ListOpt vals) _) (StringOpt val) = opt{value = ListOpt (vals++[val])}
appendValue opt _ = opt

addFlag :: Option -> Options -> Options
addFlag opt (Options opts) = Options (opt:opts)

removeFlag :: String -> Options -> Options
removeFlag str (Options opts) = Options (filter (\x -> not (isFlag (getFlagOrPrefix str) x)) opts)

getFlag :: String -> Options -> Maybe Option
getFlag str (Options opts) | length matchOpts == 0 = Nothing
                           | length matchOpts == 1 = Just (head matchOpts)
                           | otherwise = Nothing
  where matchOpts = filter (\x -> (isFlag (str) x)) opts

isFlag :: String -> Option -> Bool
isFlag "" _ = False
isFlag str option = if elem (getFlagOrPrefix str) (long (flags option))
  then True
  else elem (getFlagOrPrefix str) (short (flags option))

getFlagOrPrefix :: String -> String
getFlagOrPrefix str | elem '=' str = ((takeWhile (/= '=') str))
                    | otherwise = str


-- FILES -----------------------------------------------------------------
parseOptionFileName :: [String] -> (String,[String])
parseOptionFileName (x1:x2:xs) | length x1 == 1 = (x2,xs)--single letter
  | elem '=' x1 = (drop 1 (dropWhile (/= paramaterDelimiter) x1),(x2:xs)) -- -flag=<value>
  | otherwise = (x2,xs)--long flag with space
parseOptionFileName x = ("",x)

-- -- RANGE --------------------------------------------------------------
data GnuRange = GnuRange { min::Integer, max::Integer } deriving Show

-- -- TIME ---------------------------------------------------------------
data GnuTime = GnuTime { units::Float, unitType::TimeSuffix }
instance Show GnuTime where
  show (GnuTime unit suffix) = (show unit)++(show suffix)
instance Read GnuTime where
  readsPrec _ (x) = do (num,xx) <- readsPrec 0 x :: [(Float,String)]
                       (suff,xxxx) <- readsPrec 0 xx :: [(TimeSuffix,String)]
                       [((GnuTime num suff),xxxx)]
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

-- -- SIZE ---------------------------------------------------------------
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

