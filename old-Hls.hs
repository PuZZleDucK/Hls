


module Hls where
import System.Environment --command line args
import System.Directory
import System.FilePath
import Control.Monad
-- System.Console.Haskeline
-- System.Console.Haskeline.Completion
-- System.Console.Haskeline.History
-- System.Console.Shell
-- System.Console.Shell.Backend
-- System.Console.Shell.Backend.Basic
-- System.Console.SimpleLineEditor
-- System.Console.Terminfo
import System.Console.Terminfo.Base
-- System.Console.Terminfo.Color
import System.Console.Terminfo.Cursor
-- System.Console.Terminfo.Edit
-- System.Console.Terminfo.Effects
-- System.Console.Terminfo.Keys
import Data.Maybe

main = do
  putStrLn (" :: Haskell ls :: ")
  term <- setupTermFromEnv
  let (Just height) = getCapability term termLines
  let (Just width) = getCapability term termColumns
  args <- getArgs
  let recurse = (elem "-R" args)
  let targets = filter (not . (== "-R")) args
  let dirs = if null targets then ["."] else targets
  let showHeader = recurse || (length dirs) > 1
--  findSuidFiles (return dirs) recurse

  let fileLists = getFiles dirs
--  let displayList = formatList (fileLists) width showHeader
  let sizeDisplay = "("++(show height)++" / "++(show width)++")"

  let cr = getCapability term (carriageReturn :: (Capability TermOutput))
  let up = getCapability term (moveUp :: (Capability (Int -> TermOutput)))


  runTermOutput term (termText (" Targets: "++(show dirs)++"\n"))
  runTermOutput term (termText (" Recurse: "++(show recurse)++"\n"))
  runTermOutput term (termText (" Header row(s): "++(show showHeader)++"\n"))
--  runTermOutput term (termText (" Files: "++(((liftM show fileLists))++"\n"))
  runTermOutput term (termText (" Terminal size: "++sizeDisplay++"\n"))

--  runTermOutput term (termText (" Output: \n"++unlines (formatList (dropMaybe fileLists) width showHeader)))

  runTermOutput term ((fromMaybe (\_ -> termText "") up) 7)
  runTermOutput term (termText ("#\n"))
  runTermOutput term (termText ("#\n"))
  runTermOutput term (termText ("#\n"))
  runTermOutput term (termText ("#\n"))
  runTermOutput term (termText ("#\n"))
  runTermOutput term (termText ("#\n"))
  runTermOutput term (termText ("\n"))
  runTermOutput term (termText ("\n"))

  return ()



getFiles :: [FilePath] -> [(FilePath, (IO [String]))]
getFiles [dir] = [getFilesFromDir dir]
getFiles (dir:otherDirs) = (getFilesFromDir dir):(getFiles otherDirs)

getFilesFromDir :: FilePath -> (FilePath, IO [String])
--getFilesFromDir dir = let files <- getDirectoryContents dir in
getFilesFromDir dir = (dir, getDirectoryContents dir )
--getFilesFromDir dir = (dir, (getDirectoryContents dir))

formatList :: [(FilePath, IO [String])] -> Int -> Bool -> IO [String]
formatList fileLists@((path, listing):rest) width showHeader = listing

--dropMaybe :: [(FilePath, (Maybe [String]))] -> [(FilePath, [String])]
--dropMaybe [(path, Just strings)] = [(path, strings)]
--dropMaybe [(path, Nothing)] = [(path, [""])]
--dropMaybe ((path, Just strings):next) = (path, strings):(dropMaybe next)
--dropMaybe ((path, Nothing):next) = (path, [""]):(dropMaybe next)








{-
formatList :: [(FilePath, IO [String])] -> Int -> Bool -> IO [String]
formatList fileLists@((path, listing):rest) width showHeader = if (null fileLists) then return [""]
  else if showHeader then return (mconcat (( (++) (show path) (":")):((liftM formatFiles listing width))))
                     else return (mconcat ((liftM formatFiles listing width)))

formatFiles :: [String] -> Int -> [String]
formatFiles input width = input
-}

