-- GUtils, utils for a haskell implementation of GNU core-utils.

module GUtils where
import GOptions
--import System.Console.Terminfo.Base

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

defaultOptions :: Options
defaultOptions = Options [ helpOption
                         , versionOption
                         , targetOption ]

catOptions  :: Options -> Options -> Options
catOptions (Options opts1) (Options opts2) = Options (opts1++opts2)

replaceFlag :: Options -> String -> OptionValue -> Options
replaceFlag opts str val = addFlag (setValue (getFlag (getFlagOrPrefix str) opts) val) (removeFlag (getFlagOrPrefix str) opts)

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
parseShortOption dat str unused | length str == 1 = (dat{configuration = newCfg}, stillUnused)
                                | otherwise = parseShortOption (dat{configuration = newCfg}) (tail str) unused
  where cfg = configuration dat
        (OptionEffect effect) = paramaterEffect (getFlag ((head str):[]) cfg)
        (newCfg, stillUnused) = (effect cfg (str) unused)





