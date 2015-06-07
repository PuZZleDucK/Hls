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



blackGreyWhite = [0,5,7,8,13,15,16,58,59,60,65,94,101,102,183,182,186,195,223,224,225,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,167,172,173,181,184,185,188,190,191,192,193,194,232,229,231,187,189,230,155,156,228,100,142,143,149,150,12,151,152,153,157,158,159,35,36,23,61,66,67,73,74,48,49,72,78,79,105,108,109,110,111,114,115,120,121,95,96,97,98,99,103,104,131,132,137,138,139,140,141,145,146,168,169,170,171,174,175,176,177,204,205,206,207,210,211,212,213,217,218,219,52,88,161,162,130,198,215,216,179,180,221,222,84,29,41,42,106,107,43,147,117,116,85,63,133,134,135,129]

reds = [89,125,124,1,160,196,9,197]
oranges = [203,209,3,136,136,166,202,208]
yellows = [178,214,220,220,11,226,227]
green = [148,46,118,76,2,22,71,113] --154,83,47,119,112,40,28,64,34,77,70,,10,82
cyans = [30,38,14,50,87,123,81,62] --6,31,39,44,51,86,122,80,75,68,,69,45,37
blues = [4,24,27,32,21,20,17,56] --,57,25,26,33,18,19,
purples = [53,55,91,128,126,164,200,201] --54,90,92,93,127,165,199,163,

--rainbowPallete = concat (map (\x -> concat (take 3 (repeat x))) [reds, oranges, yellows, green, cyans, blues, purples])
rainbowPallete = concat (map (\x -> makeCopiesList x 7) [reds, oranges, yellows, green, cyans, blues, purples])

makeCopies :: Int -> Int -> [Int]
makeCopies item n = ((take n (repeat item)))

makeCopiesList :: [Int] -> Int -> [Int]
makeCopiesList [] n = []
makeCopiesList lists n = concat (map (\x -> makeCopies x n) lists)


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



