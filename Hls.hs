
module Hls where
import System.Environment
import System.Directory
import System.FilePath
import System.Posix.Files
import System.Posix.Types
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

getFileTypes :: FilePath -> IO [Maybe FileType]
getFileTypes path = do status <- getFileStatus path 
                       return [ if (isNamedPipe status) then Just FifoType else Nothing
                              , if (isCharacterDevice status) then Just CharDevType else Nothing
                              , if (isDirectory status) then Just DirectoryType else Nothing
                              , if (isBlockDevice status) then Just BlockDevType else Nothing
                              , if (isRegularFile status) then Just NormalType else Nothing
                              , if (isSymbolicLink status) then Just SymbolicLinkType else Nothing
                              , if (isSocket status) then Just SockType else Nothing
                              ]

getFileType :: FilePath -> IO FileType
getFileType path = do maybeTypes <- getFileTypes path
                      let justTypes = (catMaybes (maybeTypes))
                      if length justTypes > 1
                        then return UnknownType
                        else return (head justTypes)


--                      | otherwise = 
--linkTarget :: FilePath
--hasCapability :: Bool
--fileType :: FileType
--data FileType = UnknownType
--              | FifoType
--              | CharDevType
--              | DirectoryType
--              | BlockDevType
--              | NormalType
--              | SymbolicLinkType
--              | SockType
--              | WhiteoutType
--              | ArgDirectoryType
--linkType :: LinkType --links only
--linkOk :: Bool --links only
--containsFiles :: [FileInfo] --directories only




--add fileExist guard
--fileID status --iNode
--getFileStatus path
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
--readSymbolicLink







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
                         , linkTarget :: FilePath
                         , hasCapability :: Bool
                         , fileStatus ::  FileStatus
                         , fileType :: FileType
                         , linkType :: LinkType --links only
                         , linkOk :: Bool --links only
                         , containsFiles :: [FileInfo] --directories only
                         --, inode :: 
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

--data CommandArgs = CommandArgs { commandFlag :: String
--                               , commandName :: String
--                               , commandDescription :: String
--}


{-
--FROM GNU/LINUX LS

:: COMMAND FLAGS ::
("g", "Suppress Owner", "")
("G", "Suppress Group", "")
("o", "Suppress Group", "")
("n", "Numeric Ids", "Print the user and group id's as numbers rather than as names")
("s", "Block Size", "Mention the size in blocks of each file")
("color", "Color Types", "Use colors to mark types")
("w", "Width of listing", "")
("ignore", "Ignore", "Ignore '.', '..', and files")
("indicator-style", "", "")
("F", "indicator style", "")
("p", "indicator style", "")
("h", "Human readable options", "")
("a", "all", "")
("b", "escape", "")
("d", "directories", "")
("D", "dired", "")
("group-directories-first", "group directories first", "")
("i", "inode", "")
("k", "kibibytes", "")
("q", "hide-control-chars", "")
("r", "reverse", "")
("s", "size", "")
("A", "almost-all", "")
("B", "ignore-backups", "")
("F", "classify", "")
("H", "dereference-command-line", "")
("I", "ignore", "")
("L", "dereference", "")
("N", "literal", "")
("Q", "quote name", "")
("R", "recursive", "")
("T", "tabsize", "")
("Z", "context", "")
("full-time", "", "")
("sort", "", "")
("hide", "", "")
("indicator-style", "", "")
("quoting-style", "", "")
("format", "", "")
("time-style", "", "")
("block-size", "", "")
("si", "", "")
("author", "", "")
("color", "", "")
("time", "", "")
("show-control-chars", "", "")
("dereference-command-line-symlink-to-dir", "", "")
("file-type", "", "")


    none,       /*     --indicator-style=none */'none' means don't mention the type of files.
    slash,      /* -p, --indicator-style=slash */'slash' means mention directories only, with a '/'.
    file_type,  /*     --indicator-style=file-type */'file_type' means mention file types.
    classify    /* -F, --indicator-style=classify */'classify' means mention file types and mark executables.

echo "$LS_COLORS"
rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lz=01;31:*.xz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.axv=01;35:*.anx=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.axa=00;36:*.oga=00;36:*.spx=00;36:*.xspf=00;36:

/* A linked list of shell-style globbing patterns.  If a non-argument file name matches any of these patterns, it is ignored. Controlled by -I.  Multiple -I options accumulate. The -B option adds '*~' and '.*~' to this list.  */

/* The minimum width of a column is 3: 1 character for the name and 2 for the separating white space.  */  #define MIN_COLUMN_WIDTH        3

-}




colorArguments = ["always", "yes", "force", "never", "no", "none", "auto", "tty", "if-tty"]

timeArguments = ["atime", "access", "use", "ctime", "status"]
data TimeTypes = MTime | CTime | ATime

sortArguments = ["none", "time", "size", "extension", "version"]

formatArguments = ["verbose", "long", "commas", "horizontal", "across", "vertical", "single-column"]






