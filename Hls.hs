
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





{-
--FROM GNU/LINUX LS

static struct option const long_options[] =
  {"all", no_argument, NULL, 'a'},
  {"escape", no_argument, NULL, 'b'},
  {"directory", no_argument, NULL, 'd'},
  {"dired", no_argument, NULL, 'D'},
  {"full-time", no_argument, NULL, FULL_TIME_OPTION},
  {"group-directories-first", no_argument, NULL, GROUP_DIRECTORIES_FIRST_OPTION},
  {"human-readable", no_argument, NULL, 'h'},
  {"inode", no_argument, NULL, 'i'},
  {"kibibytes", no_argument, NULL, 'k'},
  {"numeric-uid-gid", no_argument, NULL, 'n'},
  {"no-group", no_argument, NULL, 'G'},
  {"hide-control-chars", no_argument, NULL, 'q'},
  {"reverse", no_argument, NULL, 'r'},
  {"size", no_argument, NULL, 's'},
  {"width", required_argument, NULL, 'w'},
  {"almost-all", no_argument, NULL, 'A'},
  {"ignore-backups", no_argument, NULL, 'B'},
  {"classify", no_argument, NULL, 'F'},
  {"file-type", no_argument, NULL, FILE_TYPE_INDICATOR_OPTION},
  {"si", no_argument, NULL, SI_OPTION},
  {"dereference-command-line", no_argument, NULL, 'H'},
  {"dereference-command-line-symlink-to-dir", no_argument, NULL, DEREFERENCE_COMMAND_LINE_SYMLINK_TO_DIR_OPTION},
  {"hide", required_argument, NULL, HIDE_OPTION},
  {"ignore", required_argument, NULL, 'I'},
  {"indicator-style", required_argument, NULL, INDICATOR_STYLE_OPTION},
  {"dereference", no_argument, NULL, 'L'},
  {"literal", no_argument, NULL, 'N'},
  {"quote-name", no_argument, NULL, 'Q'},
  {"quoting-style", required_argument, NULL, QUOTING_STYLE_OPTION},
  {"recursive", no_argument, NULL, 'R'},
  {"format", required_argument, NULL, FORMAT_OPTION},
  {"show-control-chars", no_argument, NULL, SHOW_CONTROL_CHARS_OPTION},
  {"sort", required_argument, NULL, SORT_OPTION},
  {"tabsize", required_argument, NULL, 'T'},
  {"time", required_argument, NULL, TIME_OPTION},
  {"time-style", required_argument, NULL, TIME_STYLE_OPTION},
  {"color", optional_argument, NULL, COLOR_OPTION},
  {"block-size", required_argument, NULL, BLOCK_SIZE_OPTION},
  {"context", no_argument, 0, 'Z'},
  {"author", no_argument, NULL, AUTHOR_OPTION},




struct fileinfo
    char *name;/* The file name.  */
    char *linkname;/* For symbolic link, name of the file linked to, otherwise zero.  */
    struct stat stat;
    enum filetype filetype;
    mode_t linkmode; /* For symbolic link and long listing, st_mode of file linked to, otherwise zero.  */
    char *scontext;/* security context.  */
    bool stat_ok;
    bool linkok;/* For symbolic link and color printing, true if linked-to file exists, otherwise false.  */
    enum acl_type acl_type;/* For long listings, true if the file has an access control list, or a security context.  */
    bool has_capability;/* For color listings, true if a regular file has capability info.  */





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


-}


data LinkModes = NoLink | HardLink | SoftLink

data IndicatorStyle = NoIndicator
                    | SlashIndicator
                    | TypeIndicator
                    | ClassifyIndicator

colorArguments = ["always", "yes", "force", "never", "no", "none", "auto", "tty", "if-tty"]

timeArguments = ["atime", "access", "use", "ctime", "status"]
data TimeTypes = MTime | CTime | ATime

sortArguments = ["none", "time", "size", "extension", "version"]
data SortType = NoSort
              | NameSort
              | ExtensionSort
              | SizeSort
              | VersionSort
              | TimeSort

formatArguments = ["verbose", "long", "commas", "horizontal", "across", "vertical", "single-column"]
data FormatTypes = LongFormat
                 | WithCommasFormat
                 | HorizontalFormat
                 | ManyPerLineFormat
                 | OnePerLine

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





