
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

calculateLength :: [String] -> Int
calculateLength [] = 0
calculateLength (x:xs) = (length x)+(calculateLength xs)

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

enum filetype
    unknown,
    fifo,
    chardev,
    directory,
    blockdev,
    normal,
    symbolic_link,
    sock,
    whiteout,
    arg_directory

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

enum format
    long_format,                /* -l and other options that imply -l */
    one_per_line,               /* -1 */
    many_per_line,              /* -C */
    horizontal,                 /* -x */
    with_commas                 /* -m */

enum time_type
    time_mtime,                 /* default */
    time_ctime,                 /* -c */
    time_atime,                 /* -u */
    time_numtypes               /* the number of elements of this enum */

enum sort_type
    sort_none = -1,             /* -U */
    sort_name,                  /* default */
    sort_extension,             /* -X */
    sort_size,                  /* -S */
    sort_version,               /* -v */
    sort_time,                  /* -t */
    sort_numtypes               /* the number of elements of this enum */

   true means the opposite order in each case.  -r  */


/* True means to display owner information.  -g turns this off.  */
static bool print_owner = true;/* True means to display author information.  */
static bool print_author;
/* True means to display group information.  -G and -o turn this off.  */
/* True means print the user and group id's as numbers rather than as names.  -n  */
static bool numeric_ids;
static bool print_block_size;/* True means mention the size in blocks of each file.  -s  */
/* -h Human-readable options for output, when printing block counts.  */
/* 'none' means don't mention the type of files.
   'slash' means mention directories only, with a '/'.
   'file_type' means mention file types.
   'classify' means mention file types and mark executables.
   Controlled by -F, -p, and --indicator-style.  */

enum indicator_style
    none,       /*     --indicator-style=none */
    slash,      /* -p, --indicator-style=slash */
    file_type,  /*     --indicator-style=file-type */
    classify    /* -F, --indicator-style=classify */

/* True means use colors to mark types.  Also define the different colors as well as the stuff for the LS_COLORS environment variable. The LS_COLORS variable is now in a termcap-like format.  */

  /* Ignore '.', '..', and files specified by --ignore.  

/* A linked list of shell-style globbing patterns.  If a non-argument file name matches any of these patterns, it is ignored. Controlled by -I.  Multiple -I options accumulate. The -B option adds '*~' and '.*~' to this list.  */

/* The line length to use for breaking lines in many-per-line format. Can be set with -w.  */


static struct option const long_options[] =
{
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

static char const *const format_args[] =
  "verbose", "long", "commas", "horizontal", "across",
  "vertical", "single-column", NULL

static enum format const format_types[] =
  long_format, long_format, with_commas, horizontal, hor
izontal,
  many_per_line, one_per_line

static char const *const sort_args[] =
  "none", "time", "size", "extension", "version", NULL

static char const *const time_args[] =
  "atime", "access", "use", "ctime", "status", NULL

static char const *const color_args[] =
  /* force and none are for compatibility with another color-ls version */
  "always", "yes", "force",
  "never", "no", "none",
  "auto", "tty", "if-tty", NULL

/* The minimum width of a column is 3: 1 character for the name and 2 for the separating white space.  */
#define MIN_COLUMN_WIDTH        3



-}





