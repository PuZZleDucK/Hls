-- -----------------------------------------------------------------------
  -- File utils... new module?
-- -----------------------------------------------------------------------
-- GFiles, utils for manipulating files for haskell version of GNU core-utils.

module GFiles where
import System.Posix
import Control.Monad
import System.Directory
--import System.FilePath
--import System.Posix.Files
--import System.Posix.Types

--blocksize: cat /proc/mounts
--               and search for blksize=
--or better yet (for block IO): stat --format=%o <target>
getFileSize :: String -> IO Integer
getFileSize str = do
  stat <- getFileStatus str
  let (COff size) = fileSize stat
  putStrLn (">>>>"++str++"{"++(show size)++"}") --dbg
  return (fromIntegral size)


onlyDirs :: [String] -> IO [String]
onlyDirs fileList = do
  dirList <- ( filterM (isDir) fileList)
  return dirList

isDir :: String -> IO Bool
isDir handle = do
  stat <- getFileStatus handle
  --isDirectory handle
  return (isDirectory stat)

onlyEmptyDirs :: [String] -> IO [String]
onlyEmptyDirs dirList = do
  emptyList <- ( filterM (isEmpty) dirList)
  return emptyList

isEmpty :: String -> IO Bool
isEmpty handle = do
  contents <- (getDirectoryContents handle)
--  putStrLn $ (show handle) ++ " has " ++ (show ((length contents)-2))
  return ((length contents) <= 2)

deleteDir :: String -> IO ()
deleteDir file = do
  System.Posix.removeDirectory file
  return ()

makeDirectory :: String -> IO ()
makeDirectory file = do
  System.Directory.createDirectory file
  return ()



