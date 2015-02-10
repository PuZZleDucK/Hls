
module Hls where
import System.Environment
import System.Directory
import System.FilePath
import Control.Monad
import System.Console.Terminfo.Base
-- System.Console.Terminfo.Color
import System.Console.Terminfo.Cursor
import Data.Maybe
import Data.List

main = do
--  putStrLn (" :: Haskell ls :: ")
  term <- setupTermFromEnv
  let (Just height) = getCapability term termLines
  let (Just width) = getCapability term termColumns
  args <- getArgs
  let recurse = (elem "-R" args)
  let targets = filter (not . (== "-R")) args
  let dirs = if null targets then ["."] else targets
  let showHeader = recurse || (length dirs) > 1

  let fileLists = getFiles dirs
--  let displayList = formatList (fileLists) width showHeader
  let sizeDisplay = "("++(show height)++" / "++(show width)++")"

  let cr = getCapability term (carriageReturn :: (Capability TermOutput))
  let up = getCapability term (moveUp :: (Capability (Int -> TermOutput)))


  runTermOutput term (termText (" \tTargets: "++(show dirs)++"\n"))
  runTermOutput term (termText (" \tRecurse: "++(show recurse)++"\n"))
--  runTermOutput term (termText (" Header row(s): "++(show showHeader)++"\n"))
--  runTermOutput term (termText (" Files: "++(((liftM show fileLists))++"\n"))
  runTermOutput term (termText (" \tTerminal size: "++sizeDisplay++"\n"))
--  runTermOutput term (termText (" Sample header: "++(show (formatList sampleOutput 82 True))++"\n"))
--  runTermOutput term (termText (" Sample no header thin: "++(show (formatList sampleOutput 5 False))++"\n"))


--  runTermOutput term (termText (foldr (++) "\n" (formatList sampleOutput width recurse)))

  sequence (map (flip displayOutput term) (formatList sampleOutput width recurse))

--  runTermOutput term (termText (" Output: \n"++unlines (formatList (dropMaybe fileLists) width showHeader)))

--  runTermOutput term ((fromMaybe (\_ -> termText "") up) 7)
--  runTermOutput term (termText ("#\n"))
  return ()


displayOutput :: String -> Terminal -> IO ()
displayOutput str term = runTermOutput term (termText (str++"\n"))

sampleOutput :: [(FilePath,[String])]
sampleOutput =  [(".",["FileUtils.hs", "Hls.hs~", "pedantic.log", "Hls.hs", "old-Hls.hs", "SuidFinder.hs"]),("/",["bin", "home", "lost+found", "proc", "selinux", "usr", "boot", "initrd.img", "media", "root", "srv", "var", "dev", "initrd.img.old", "mnt", "run", "sys", "vmlinuz", "etc", "lib", "opt", "sbin", "tmp", "vmlinuz.old"])]

getFiles :: [FilePath] -> [(FilePath, (IO [String]))]
getFiles [dir] = [getFilesFromDir dir]
getFiles (dir:otherDirs) = (getFilesFromDir dir):(getFiles otherDirs)

--sequence not correct, default should be  alpha top-down then left-right
--current is all over the shop
--now sorted, but going l2r then u2d... want transpose?!?!
getFilesFromDir :: FilePath -> (FilePath, IO [String])
getFilesFromDir dir = (dir, getDirectoryContents dir )

--need to work out spacing before knowing list numbers and orders
--use trial and error
--assume 1 row
--calculate width using 1 row    <--|
--if row width > screenWidth        |
--      increment row count --------|
--if row count > height use paging
--else pad & display

--                   input   display    rows
calculateRowCount :: [String] -> Int -> Int
calculateRowCount list displayWidth = 0


--                                   Width   showHeader
formatList :: [(FilePath, [String])] -> Int -> Bool -> [String]
formatList [] width header = []
formatList lists@((path, listing):rest) width header = if header
  then ((path++":"):formattedEntries)++(formatList rest width header)
  else formattedEntries++(formatList rest width header)
    where entryMaxWidth = (widestFilename listing)+1
          sortedEntries = sort listing
          formattedEntries = formatListing sortedEntries entryMaxWidth width


formatListing :: [String] -> Int -> Int -> [String]
formatListing [] entryMaxWidth width = []
formatListing listing entryMaxWidth width
  | entriesPerRow < 2 = listing
  | (length listing) < entriesPerRow = (foldr ((++)) "" (paddedEntries)):[]
  | otherwise = foldr (++) "" (take entriesPerRow paddedEntries) : (formatListing (drop entriesPerRow listing) entryMaxWidth width)
    where entriesPerRow = width `div` entryMaxWidth
          paddedEntries = map ((flip padDisplayString) entryMaxWidth) listing


widestFilename :: [String] -> Int
widestFilename [] = 0
widestFilename list = max ((length . head) list) (widestFilename (tail list))


padDisplayString :: String -> Int -> String
padDisplayString input targetLength | (length input) >= targetLength = input
                                    | otherwise = input ++ take (targetLength-(length input)) (repeat '_')


