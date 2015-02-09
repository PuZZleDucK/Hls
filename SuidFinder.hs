{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module SuidFinder where
import System.Directory
import Control.Monad
import System.FilePath
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Data.Int
import qualified Data.ByteString.Lazy as L
import System.Posix.Directory
import System.Posix.Files
import System.Environment --command line args
import System.IO          --file handle handling
import Control.Monad.IO.Class -- liftIO
 
main = do
  putStrLn (" :: SUID Finder :: ")

  args <- getArgs
  let recurse = (elem "-R" args)
      dirs = if null targets then ["/"] else targets
        where targets = filter (not . (== "-R")) args
  
  findSuidFiles (return dirs) recurse
  
  return ()
--   -R ==> recursive

  


--type tangled... I've gotten my IOs all over the place
--findSuidFiles :: [Handle] -> Bool -> IO [Handle]
--findSuidFiles (thisPath:otherPaths) recurse = do
--  listing <- getFileList thisPath
--  suidList <- ioFilter isSuid listing
--  subs <- getSubDirs thisPath
--  --subList = (++) otherPaths subs
--  if recurse then return (concat ( [suidList] ++ findSuidFiles (otherPaths ++ subs) recurse))
--             else return (concat ( [suidList] ++ findSuidFiles otherPaths recurse))

findSuidFiles :: IO [FilePath] -> Bool -> IO [FilePath]
findSuidFiles paths recurse = do
  (thisPath:otherPaths) <- paths
  thisHandle <- openFile thisPath ReadMode
  listing <- getFileList thisHandle
  status <- getFileStatus thisHandle
  --suidList <- ioFilter isSuid listing
  fileMode = fileMode status    -- setUserIDMode & setGroupIDMode 
  hClose thisHandle
  subs <- getSubDirs thisPath
  otherSuids <- if recurse then findSuidFiles (return otherPaths) recurse
                           else findSuidFiles (return (otherPaths++subs)) recurse
  return ((suidList) ++ (otherSuids))


ioFilter :: (a -> IO Bool) -> [a] -> IO [a]
ioFilter = undefined

isSuid :: Handle -> IO Bool
isSuid = undefined

getFileList :: Handle -> IO [Handle]
getFileList = undefined

getSubDirs :: Handle -> IO [Handle]
getSubDirs = undefined

{-
setUserIDMode
System.Posix.Files.fileAccess
System.Posix.Files.fileGroup
System.Posix.Files.fileMode
System.Posix.Files.fileOwner
System.Posix.Files.fileTypeModes
System.Posix.Files.fileSize
System.Posix.Files.groupExecuteMode
System.Posix.Files.groupModes
System.Posix.Files.isBlockDevice
System.Posix.Files.isCharacterDevice
System.Posix.Files.isDirectory
System.Posix.Files.isNamedPipe
System.Posix.Files.isRegularFile
System.Posix.Files.isSocket
System.Posix.Files.isSymbolicLink
System.Posix.Files.otherExecuteMode
System.Posix.Files.otherModes
-}


-- -------------------------------------------------------------------




listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter noDots) . getDirectoryContents
  where noDots p = p /= "." && p /= ".."






