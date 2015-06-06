-- Hlolcat, a haskell implementation of lolcat.
module Main where
import System.Console.Terminfo.Base
import System.Console.Terminfo.Color
import System.Console.Terminfo.Cursor
import System.Environment
import GUtils
import GOptions
import GFiles
import Data.Maybe
import Data.List
import System.Process

main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs
  let height = fromMaybe 0 (getCapability term termLines)
      width = fromMaybe 0 (getCapability term termColumns)

  let up = getCapability term (moveUp :: (Capability (Int -> TermOutput)))
  let fgColor = getCapability term (setForegroundColor :: (Capability (Color -> TermOutput)))
  let clearColors = getCapability term (restoreDefaultColors :: (Capability (TermOutput)))


  putStrLn ":::" --temp
  let defaultConfig = ProgramData {
    appName = "Hlolcat"
  , appHelp = customHelp
  , appVersion = customVersion
  , argumentStrings = args
  , configuration = customOptions
  }
  let config = parseArguments defaultConfig args
  putStr (   showHelp config)
  putStr (showVersion config)

  input <- getContents -- buffering
  let inLines = lines input;
  let (Just fg) = fgColor
      red = fg Red
      pallete = map fg (map ColorNumber [0..8]) --realy limited colors :(
      longPallete = map fg  (map ColorNumber (concat (map ((take 8) . repeat) [0..8]))) --realy limited colors :(
--  runTermOutput term red


--  let ccode = ("echo -e '\033[0m'") -- \033[36m -- 1b[93;41m --
  cyan <- readProcess "echo" ["-ne", "\\033[36m"] ""
  cyan1 <- readProcess "echo" ["-ne", "\\033[35m"] ""
  cyan2 <- readProcess "echo" ["-ne", "\\033[34m"] ""
  let customPallete = [cyan, cyan1, cyan2]
      cPalleteStrings = map (\x -> "\\033[38;5;"++(pad 2 '0' x)++"m") ([0..250]++(take 255 (repeat 36))++rainbowPallete++(take 255 (repeat 1)))
--      cPalleteStrings = map (\x -> "\\033["++(pad 2 '0' x)++"m") [10..20]
  cPallete <- mapM (\x -> readProcess "echo" ["-ne", x] "") cPalleteStrings
--  runTermOutput term (termText ("color>"++(cyan)++"<color\n"))
  
  
--  runTermOutput term (termText ("Input: "++(show  input)++"\n"))
--  sequence (map (\x -> runTermOutput term (termText ("#"++(show x)++"\n"))) inLines)
--  let cLines = zip (concat (repeat pallete)) inLines
--  sequence (map (\(x, y) -> putTermColor term x ("#"++(show y))) cLines)
  
--  let ccLines = concat (map (\x -> zip (concat (repeat pallete)) x) inLines)
--  sequence (map (\(x, y) -> putTermColorChar term x (y:[])) ccLines)

  let ccContents = zip (concat (repeat longPallete)) input
      ccCustom = zip (concat (repeat customPallete)) input
      cCustomPallete = zip (concat (repeat (cPallete))) input
--  sequence (map (\(x, y) -> putTermColorChar term x (y:[])) ccContents)
--  sequence (map (\(x, y) -> putTermColorChar term (termText x) (y:[])) ccCustom)

--  putStrLn ("\n\nCP: "++(show cPalleteStrings))
  
  sequence (map (\(x, y) -> putTermColorChar term (termText x) (y:[])) cCustomPallete)

  let (Just jClearColors) = clearColors
  runTermOutput term jClearColors
--  putStrLn ("\n\n"++(show config)++"\n") --debug opts
  return ()



blackGreyWhite = [0,5,7,8,13,15,16,58,59,60,65,94,101,102,183,182,186,195,223,224,225,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,167,172,173,181,184,185,188,190,191,192,193,194,232,229,231,187,189,230,155,156,228,100,142,143,149,150,12,151,152,153,157,158,159,35,36,23,61,66,67,73,74,48,49,72,78,79,105,108,109,110,111,114,115,120,121,95,96,97,98,99,103,104,131,132,137,138,139,140,141,145,146,168,169,170,171,174,175,176,177,204,205,206,207,210,211,212,213,217,218,219]

reds = [10,10,10,10,10,10,10,10, 1,9,52,88,124,160,196,89,125,161,162,197,198                                                      ,10,10,10,10,10,10,10,10]
oranges = [10,10,10,10,10,10,10,10,              3,130,136,166,202,203,208,209,214,215,216     ,10,10,10,10,10,10,10,10]
yellows = [11,178,179,180,220,221,222,227,226     ,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10]
green = [1,1,1,1,1,1,1,1,                   2,10,22,28,34,40,41,42,46,47,64,70,76,71,77,82,83,106,107,112,113,118,119,148,154,84,29      ,1,1,1,1,1,1,1,1]
cyans = [1,1,1,1,1,1,1,1,                               6,14,30,31,38,39,43,44,45,51,    62,68,69,75,50,80,81,   85,86,87,116,117,122,123,37,147     ,1,1,1,1,1,1,1,1]
blues = [4,17,18,19,20,21,25,26,27,32,63,24,33,56,57     ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
purples = [53,54,55,90,91,92,93,126,127,128                                     ,129,133,134,135      ,163,164,165,199,200,201     ,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10]

rainbowPallete = reds++oranges++yellows++green++cyans++blues++purples



pad :: Int -> Char -> Int -> String
pad cnt padChar num | length sNum >= cnt = sNum
                    | otherwise = (take (cnt-(length sNum)) (repeat padChar))++sNum
  where sNum = show num


putTermColor :: Terminal -> TermOutput -> String -> IO ()
putTermColor term color string = do
  runTermOutput term color
  runTermOutput term (termText ("#"++(show string)++"\n"))

putTermColorChar :: Terminal -> TermOutput -> String -> IO ()
putTermColorChar term color ch = do
  runTermOutput term color
  runTermOutput term (termText ((ch)))

someOption :: Option
someOption = Option "<Op text>"
  (Flags ["s"] ["long-flag"])
  "=<params>"
  (BoolOpt False)
  (OptionEffect (\(opts) _ unused -> ((replaceFlag opts "long-flag" (BoolOpt True)), unused)))

customOptions :: Options
customOptions = catOptions (Options [
  someOption
  ]) defaultOptions

customVersion :: String
customVersion = "Hlolcat, a Haskell clone of lolcat."
  ++ "\n"
  ++ "\nPorted to Haskell by PuZZleDucK."

customHelp :: (String,String)
customHelp = ("Usage: Hlolcat"
              ++ "\nand enjoy the color."
             ,"\nother command line options are ignored."
             )



