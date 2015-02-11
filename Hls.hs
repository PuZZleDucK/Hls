
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

data FileInfo = FileInfo { name :: String
                         , linkTarget :: FilePath
                         --, stat :: 
                         , fileType :: FileType
                         , linkType :: LinkType
                         --, securityContext :: 
                         , linkOk :: Bool
                         --, accessControlList ::
                         , hasCapability :: Bool
}
data FileType = UnknownType
              | FifoType
              | CharDevType
              | DirectoryType
              | BlockDevType
              | NormalType
              | SymbolicLinkType
              | SockType
              | WhiteoutType
              | ArgDirectoryType
data LinkType = NoLink | HardLink | SoftLink

data CommandArgs = CommandArgs { commandFlag :: String
                               , commandName :: String
                               , commandDescription :: String

}

commandArgs = map (((uncurry.uncurry)) CommandArgs) [ (("g", "Suppress Owner"), "c") ]

{-
--FROM GNU/LINUX LS
/* True means to display owner information.  -g turns this off.  */
static bool print_owner = true;/* True means to display author information.  */
static bool print_author;
/* True means to display group information.  -G and -o turn this off.  */
/* True means print the user and group id's as numbers rather than as names.  -n  */
static bool numeric_ids;
static bool print_block_size;/* True means mention the size in blocks of each file.  -s  */
/* 
   
   
   
   

/* True means use colors to mark types.  Also define the different colors as well as the stuff for the LS_COLORS environment variable. The LS_COLORS variable is now in a termcap-like format.  */
echo "$LS_COLORS"
rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lz=01;31:*.xz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.axv=01;35:*.anx=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.axa=00;36:*.oga=00;36:*.spx=00;36:*.xspf=00;36:

/* A linked list of shell-style globbing patterns.  If a non-argument file name matches any of these patterns, it is ignored. Controlled by -I.  Multiple -I options accumulate. The -B option adds '*~' and '.*~' to this list.  */


/* The minimum width of a column is 3: 1 character for the name and 2 for the separating white space.  */
#define MIN_COLUMN_WIDTH        3


:: COMMAND FLAGS ::
-w   line length to use for breaking lines in many-per-line format. Can be set with
--ignore   Ignore '.', '..', and files
--indicator-style   Controlled by -F, -p, and --indicator-style.  */
    none,       /*     --indicator-style=none */'none' means don't mention the type of files.
    slash,      /* -p, --indicator-style=slash */'slash' means mention directories only, with a '/'.
    file_type,  /*     --indicator-style=file-type */'file_type' means mention file types.
    classify    /* -F, --indicator-style=classify */'classify' means mention file types and mark executables.
/* -h Human-readable options for output, when printing block counts.  */
enum sort_type
    sort_none = -1,             /* -U */
    sort_name,                  /* default */
    sort_extension,             /* -X */
    sort_size,                  /* -S */
    sort_version,               /* -v */
    sort_time,                  /* -t */
   true means the opposite order in each case.  -r  */
enum time_type
    time_mtime,                 /* default */
    time_ctime,                 /* -c */
    time_atime,                 /* -u */
enum format
    long_format,                /* -l and other options that imply -l */
    one_per_line,               /* -1 */
    many_per_line,              /* -C */
    horizontal,                 /* -x */
    with_commas                 /* -m */


Flags
"all" 'a'
"escape" 'b'
"directory" 'd'
"dired" 'D'
"group-directories-first" GROUP_DIRECTORIES_FIRST_OPTION
"human-readable" 'h'
"inode" 'i'
"kibibytes" 'k'
"numeric-uid-gid" 'n'
"no-group" 'G'
"hide-control-chars" 'q'
"reverse" 'r'
"size" 's'
"width" 'w'
"almost-all" 'A'
"ignore-backups" 'B'
"classify" 'F'
"file-type" FILE_TYPE_INDICATOR_OPTION
"dereference-command-line" 'H'
"dereference-command-line-symlink-to-dir" DEREFERENCE_COMMAND_LINE_SYMLINK_TO_DIR_OPTION
"ignore" 'I'
"dereference" 'L'
"literal" 'N'
"quote-name" 'Q'
"recursive" 'R'
"show-control-chars" SHOW_CONTROL_CHARS_OPTION
"tabsize" 'T'
"time" TIME_OPTION
"color" COLOR_OPTION
"context" 'Z'
"author" AUTHOR_OPTION

long_options[] =
"full-time" FULL_TIME_OPTION
"si" SI_OPTION
"hide" HIDE_OPTION
"indicator-style" INDICATOR_STYLE_OPTION
"quoting-style" QUOTING_STYLE_OPTION
"format" FORMAT_OPTION
"sort" SORT_OPTION
"time-style" TIME_STYLE_OPTION
"block-size" BLOCK_SIZE_OPTION




-}




colorArguments = ["always", "yes", "force", "never", "no", "none", "auto", "tty", "if-tty"]

timeArguments = ["atime", "access", "use", "ctime", "status"]
data TimeTypes = MTime | CTime | ATime

sortArguments = ["none", "time", "size", "extension", "version"]

formatArguments = ["verbose", "long", "commas", "horizontal", "across", "vertical", "single-column"]






