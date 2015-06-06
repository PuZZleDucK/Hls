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
      cPalleteStrings = map (\x -> "\\033[38;5;"++(pad 2 '0' x)++"m") [0..99]
--      cPalleteStrings = map (\x -> "\\033["++(pad 2 '0' x)++"m") [10..20]
  cPallete <- mapM (\x -> readProcess "echo" ["-ne", x] "") cPalleteStrings
  runTermOutput term (termText ("color>"++(cyan)++"<color\n"))
  
  
--  runTermOutput term (termText ("Input: "++(show  input)++"\n"))
--  sequence (map (\x -> runTermOutput term (termText ("#"++(show x)++"\n"))) inLines)
--  let cLines = zip (concat (repeat pallete)) inLines
--  sequence (map (\(x, y) -> putTermColor term x ("#"++(show y))) cLines)
  
--  let ccLines = concat (map (\x -> zip (concat (repeat pallete)) x) inLines)
--  sequence (map (\(x, y) -> putTermColorChar term x (y:[])) ccLines)

  let ccContents = zip (concat (repeat longPallete)) input
      ccCustom = zip (concat (repeat customPallete)) input
      cCustomPallete = zip (concat (repeat cPallete)) input
--  sequence (map (\(x, y) -> putTermColorChar term x (y:[])) ccContents)
--  sequence (map (\(x, y) -> putTermColorChar term (termText x) (y:[])) ccCustom)

--  putStrLn ("\n\nCP: "++(show cPalleteStrings))
  
  sequence (map (\(x, y) -> putTermColorChar term (termText x) (y:[])) cCustomPallete)

  let (Just jClearColors) = clearColors
  runTermOutput term jClearColors
--  putStrLn ("\n\n"++(show config)++"\n") --debug opts
  return ()

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



