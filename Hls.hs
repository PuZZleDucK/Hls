
module Main where
import System.Environment
import System.Directory
import System.FilePath
import System.Posix.Files
import System.Posix.Types
import System.Console.Terminfo.Base
-- System.Console.Terminfo.Color
import System.Console.Terminfo.Cursor
import Data.List
import Data.Maybe

main :: IO ()
main = do
  term <- setupTermFromEnv
  let height = fromMaybe 0 (getCapability term termLines)
      width = fromMaybe 0 (getCapability term termColumns)
  args <- getArgs
  let recurse = (elem "-R" args)
  let targets = filter (not . (== "-R")) args
  let dirs = if null targets then ["."] else targets

--  let cr = getCapability term (carriageReturn :: (Capability TermOutput))
--  let up = getCapability term (moveUp :: (Capability (Int -> TermOutput)))

  runTermOutput term (termText (" \tTargets: "++(show dirs)++"\n"))
  runTermOutput term (termText (" \tRecurse: "++(show recurse)++"\n"))
  runTermOutput term (termText (" \tTerminal size: "++("("++(show height)++" / "++(show width)++")")++"\n"))

--  runTermOutput term (termText (" Sample header: "++(show (formatList sampleOutput 82 True))++"\n"))
--  runTermOutput term (termText (" Sample no header thin: "++(show (formatList sampleOutput 5 False))++"\n"))


--  runTermOutput term (termText (foldr (++) "\n" (formatList sampleOutput width recurse)))

  _ <- sequence (map (flip displayOutput term) (formatList sampleOutput width recurse))

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
getFiles [] = []

--sequence not correct, default should be  alpha top-down then left-right
--current is all over the shop
--now sorted, but going l2r then u2d... want transpose?!?!
getFilesFromDir :: FilePath -> (FilePath, IO [String])
getFilesFromDir dir = (dir, getDirectoryContents dir)

--need to work out spacing before knowing list numbers and orders
--use trial and error
--assume 1 row
--calculate min width using 1 row    <--|
--if row width > screenWidth            |
--      increment row count ------------|
--else pad & display

--                   input   display   try-rows    rows
calculateRowCount :: [String] -> Int -> Int -> Int
calculateRowCount list rows displayWidth = if calculateLength list < displayWidth
  then rows
  else calculateRowCount list (rows+1) displayWidth

columnBufferWidth :: Int
columnBufferWidth = 2

calculateLength :: [String] -> Int
calculateLength [] = 0
calculateLength (x:xs) = ((length x)+columnBufferWidth)+(calculateLength xs)

--                                   Width   showHeader
formatList :: [(FilePath, [String])] -> Int -> Bool -> [String]
formatList [] _width _header = []
formatList ((path, listing):rest) width header = if header
  then ((path++":"):formattedEntries)++(formatList rest width header)
  else formattedEntries++(formatList rest width header)
    where entryMaxWidth = (widestFilename listing)+1
          sortedEntries = sort listing
          formattedEntries = formatListing sortedEntries entryMaxWidth width


formatListing :: [String] -> Int -> Int -> [String]
formatListing [] _entryMaxWidth _width = []
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


--getListings :: [FilePath] -> LsOptions -> [(FilePath, (IO [String]))]
--getListings [target] opts = getListing target opts
--getListings (target:others) opts = (getListing target opts)++(getListings others opts)

--getListing :: FilePath -> LsOptions -> [(FilePath, (IO [String]))]
--getListing target opts = [(target, getDirectoryContents target)]

--getFileInfo :: FilePath -> LsOptions -> IO FileInfo
--getFileInfo path opts = do status <- getFileStatus path
--                           let isPipe = (isNamedPipe status)
--                           if(isPipe) then return FileInfo{ base = takeBaseName path, fileType = FifoType }
--                                      else return FileInfo{ base = takeBaseName path, extention = path }


dropBase :: FilePath -> String
dropBase path | baseLength < totalLength = drop (baseLength+1) path
              | otherwise = ""
  where baseLength = length (takeBaseName path)
        totalLength = length path

getFileInfo :: FilePath -> LsOptions -> IO FileInfo
getFileInfo path _opts = do thisStatus <- getFileStatus path
                            thisType <- getFileType thisStatus
                            thisTarget <- readSymbolicLink path
--                            thisLinkType <- 
                            return FileInfo { base = takeBaseName path
                                            , extention = (dropBase path)
                                            , fileType = thisType
                                            , inode = fileID thisStatus
                                            , linkTarget = thisTarget
                                            , linkType = NoLink
                                            , hasCapability = False
                                            , linkOk = False
                                            , containsFiles = []
                                            }

getFileTypes :: FileStatus -> IO [Maybe FileType]
getFileTypes status = do
                              return [ if (isNamedPipe status) then Just FifoType else Nothing
                                     , if (isCharacterDevice status) then Just CharDevType else Nothing
                                     , if (isDirectory status) then Just DirectoryType else Nothing
                                     , if (isBlockDevice status) then Just BlockDevType else Nothing
                                     , if (isRegularFile status) then Just NormalType else Nothing
                                     , if (isSymbolicLink status) then Just SymbolicLinkType else Nothing
                                     , if (isSocket status) then Just SockType else Nothing
                                     ]

getFileType :: FileStatus -> IO FileType
getFileType status = do maybeTypes <- getFileTypes status
                        let justTypes = (catMaybes (maybeTypes))
                        if length justTypes > 1
                          then return UnknownType
                          else return (head justTypes)


--                      | otherwise = 
--linkTarget :: FilePath
--hasCapability :: Bool
--linkType :: LinkType --links only
--linkOk :: Bool --links only
--containsFiles :: [FileInfo] --directories only




--add fileExist guard
-- status --iNode
--getSymbolicLinkStatus
--fileMode
--fileOwner
--fileGroup
--fileSize
--accessTime
--modificationTime
--statusChangeTime
--
--
--
--
--
--
--
--







data LsOptions = LsOptions { listAll :: Bool
                           , listAlmostAll :: Bool
                           , listBackups :: Bool
                           , listRecursive :: Bool
                           , showInode :: Bool
                           , showSize :: Bool
                           , showClassifier :: Bool
                           , showIndicatorStyle :: IndicatorStyle
                           , showControlChars :: Bool
                           , showTime :: Bool
                           , showColor :: Bool
                           , showContext :: Bool
                           , showAuthor :: Bool
                           -- hideOptions :: 
                           , hideGroup :: Bool
                           , hideControlChars :: Bool
                           , format :: FormatStyle
                           -- formatTime ::
                           -- timeFull :: 
                           , tabSize :: Int
                           , wideListing :: Bool
                           , sortReverse :: Bool
                           , sortType :: SortType
                           , groupDirectories :: Bool
                           , escape :: Bool
                           , directory :: Bool
                           , dired :: Bool
                           , useHumanReadable :: Bool
                           , useKilobytes :: Bool
                           , useNumericIds :: Bool
                           -- si :: 
                           -- derefCommand :: Bool
                           -- derefCommandSymlink :: Bool
                           -- ignoreCommand :: 
                           -- dereference :: Bool
                           -- literal :: Bool
                           -- quote :: Bool
                           -- quoteStyle ::
                           -- blockSize :: 
}
data IndicatorStyle = NoIndicator
                    | SlashIndicator
                    | TypeIndicator
                    | ClassifyIndicator
data FormatStyle = LongFormat
                 | WithCommasFormat
                 | HorizontalFormat
                 | ManyPerLineFormat
                 | OnePerLine
data SortType = NoSort
              | NameSort
              | ExtensionSort
              | SizeSort
              | VersionSort
              | TimeSort

data FileInfo = FileInfo { base :: String
                         , extention :: String
                         , inode :: FileID
                         , linkTarget :: FilePath
                         , hasCapability :: Bool
--                         , fileStatus ::  FileStatus --this is what i'm culling
                         , fileType :: FileType
                         , linkType :: LinkType --links only
                         , linkOk :: Bool --links only
                         , containsFiles :: [FileInfo] --directories only
                         --, accessControlList ::
                         --, securityContext :: 
                         --, stat :: 
}
data FileType = UnknownType
              | FifoType
              | CharDevType
              | DirectoryType
              | BlockDevType
              | NormalType
              | SymbolicLinkType
              | SockType
              deriving (Show, Eq)
--              | WhiteoutType --no unionfs support for now
--              | ArgDirectoryType --couldn't even find this one
data LinkType = NoLink | HardLink | SoftLink


colorArguments :: [String]
colorArguments = ["always", "yes", "force", "never", "no", "none", "auto", "tty", "if-tty"]

timeArguments :: [String]
timeArguments = ["atime", "access", "use", "ctime", "status"]
data TimeTypes = MTime | CTime | ATime

sortArguments :: [String]
sortArguments = ["none", "time", "size", "extension", "version"]

formatArguments :: [String]
formatArguments = ["verbose", "long", "commas", "horizontal", "across", "vertical", "single-column"]






