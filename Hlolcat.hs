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

--  putStrLn ":::" --debug
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

  rPallete <- mapM (\x -> readProcess "echo" ["-ne", x] "") rainbowPalleteStrings
  let longPallete = concat (repeat (rPallete))

--  let cCustomPallete = zip (concat (repeat (cPallete))) input
  let colorCharPairs = (map (zip longPallete) inLines)
--add offset for each line
--  sequence (map (\(x, y) -> putTermColorChar term (termText x) (y:[])) cCustomPallete)
--  sequence (map (\(x, y) -> putTermColorChar term (termText x) (y)) cCustomPallete)

  putStrLn "sequence"
  sequence (map (\x -> putColorPairLists term x) colorCharPairs)
  putStrLn "post"

--would be nice to put this in after ^c / abort too.
  let (Just jClearColors) = clearColors
  runTermOutput term jClearColors
  return ()

putColorPairLists :: Terminal -> [(String,Char)] -> IO ()
putColorPairLists term lists = do
  sequence (map (putColorPair term) lists)
  putStrLn ""
  return ()


putColorPair :: Terminal -> (String, Char) -> IO ()
putColorPair term (color, ch) = do
  runTermOutput term (termText color)
  if (ch == '\n') then putStrLn ""
        else runTermOutput term (termText ((ch:[])))
  --need flush



reds = [89,125,124,1,160,196,9,197]
oranges = [203,209,3,136,136,166,202,208]
yellows = [178,214,220,220,11,226,227]
green = [148,46,118,76,2,22,71,113]
cyans = [30,38,14,50,87,123,81,62]
blues = [4,24,27,32,21,20,17,56]
purples = [53,55,91,128,126,164,200,201]

rainbowPallete = concat (map (\x -> makeCopiesList x 7) [reds, oranges, yellows, green, cyans, blues, purples])
rainbowPalleteStrings = map (\x -> "\\033[38;5;"++(pad 2 '0' x)++"m") (rainbowPallete)
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

