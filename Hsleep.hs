-- Hsleep, a haskell implementation of GNU sleep.
module Main where
import System.Environment
import GUtils
import System.Posix.Process
import Foreign.C.Types
import System.Time

main :: IO ()
main = do
  args <- getArgs

  let defaultConfig = ProgramData {
    appName = "Hsleep"
  , appHelp = customHelp
  , appVersion = customVersion
  , argumentStrings = args
  , configuration = customOptions
  , parser = undefined -- needed anymore??
  }
  let config = parseArguments defaultConfig args
  putStr (   showHelp config)
  putStr (showVersion config)
  
  doWork config
--  putStrLn ("\n\n"++(show config)++"\n") --debug opts
  return ()

timeMultiplier :: Float
timeMultiplier = 100

doWork :: ProgramData -> IO ()
doWork dat = do
--  putStrLn $ "Vals:" ++ (show values)
--  putStrLn $ "Sum:" ++ (show (sum values))
--  x <- getProcessTimes
--  let now = elapsedTime x
--      (CClock nowT) = now
--      nowI = fromInteger (toInteger nowT)
  xx <- getClockTime
  --not accurate timing... might need to find nanosecond solution
--  waitUntill (nowI + ((sum floatValues)*timeMultiplier))
  --pico maybe
  putStrLn ("Start: " ++ (show xx))
  putStrLn ("Floats: " ++ (show (sum floatValues)))
  waitFromTime xx (sum floatValues*100000000)
    where cfg = configuration dat
          targets = getFlag "--" cfg
          targetList = getList targets
          values = (map read targetList) :: [GnuTime]
          floatValues = map timeToFloat values
--TimeDiff tdYear :: Int, tdMonth :: Int, tdDay :: Int, tdHour :: Int, tdMin :: Int, tdSec :: Int, tdPicosec :: Integer

waitFromTime :: ClockTime -> Float -> IO ()
waitFromTime startTime cnt = do
  now <- getClockTime
  let elapsed = diffClockTimes now startTime
      (TimeDiff y m d h mi s p) = elapsed
      elapsedPicos = (fromIntegral p) + ((fromIntegral s)*10.0) 
                     + ((fromIntegral mi)*10.0*60)
                     + ((fromIntegral h)*10.0*60*60)
                     + ((fromIntegral d)*10.0*60*60*24)
                     + ((fromIntegral m)*10.0*60*60*24*30) --approximation?
                     + ((fromIntegral y)*10.0*60*60*24*30*256) :: Float
  putStrLn ("Elapsed Picos: " ++ (show elapsedPicos))
  if (elapsedPicos) >= cnt then return () else waitFromTime startTime cnt
  return ()

waitUntill :: Float -> IO ()
waitUntill targetTime = do
  pTime <- getProcessTimes
  let now = elapsedTime pTime
      (CClock nowClock) = now
      nowIint = fromInteger (toInteger nowClock)
  if nowIint > targetTime then return () else waitUntill targetTime


customOptions :: Options
customOptions = catOptions (Options [
  ]) defaultOptions

customVersion :: String
customVersion = "Hsleep, a Haskell clone of GNU sleep."
  ++ "\nsleep (GNU coreutils) 8.23.126-99f76"
  ++ "\nCopyright (C) 2015 Free Software Foundation, Inc."
  ++ "\nLicense GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
  ++ "\nThis is free software: you are free to change and redistribute it."
  ++ "\nThere is NO WARRANTY, to the extent permitted by law."
  ++ "\n\nWritten by Jim Meyering and Paul Eggert."
  ++ "\nPorted to Haskell by PuZZleDucK."

customHelp :: (String,String)
customHelp = ("Usage: H<app> ...."
              ++ "\nUsage: /home/bminerds/x/coreutils/src/sleep NUMBER[SUFFIX]..."
              ++ "\n  or:  /home/bminerds/x/coreutils/src/sleep OPTION"
              ++ "\nPause for NUMBER seconds.  SUFFIX may be 's' for seconds (the default), 'm' for minutes, 'h' for hours or 'd' for days.  Unlike most implementations that require NUMBER be an integer, here NUMBER may be an arbitrary floating point number. Given two or more arguments, pause for the amount of time specified by the sum of their values.\n"
             ,"\nGNU coreutils online help: <http://www.gnu.org/software/coreutils/>"
             ++ "\nFull documentation at: <http://www.gnu.org/software/coreutils/sleep> or available locally via: info '(coreutils) sleep invocation'"
             )



