{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}
-- NoMonomorphismRestriction for lifted operators

-- More RWH fileStuff -------------------------------------------------
module Main where
import Control.Monad --(liftM2, replicateM)
import System.Environment
import Data.List
import Data.Char (ord)
import Data.Bits (shiftL, (.&.), (.|.))
import System.FilePath
import System.Directory

import System.Time
import System.IO
import Control.Exception

main =
  do
    putStrLn "  ---------------------- "
    putStrLn " -- Haskell file tools -- "
    putStrLn "  ---------------------- "

    putStrLn ("Recursive File Listing")
    myFiles <- (getRecursiveFiles "/home/bminerds/temp/")
    sequence (take 3(map (putStrLn) myFiles))
--









-- Directory Scanning  -------------------------------------------------
data FileInfo = FileInfo { infoPath :: FilePath
                         , infoPermissions :: Maybe Permissions
                         , infoSize :: Maybe Integer
                         , infoTime :: Maybe ClockTime
                         } deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO FileInfo
getInfo fileName = do
         perms <- maybeIO (getPermissions fileName)
         size <- maybeIO (bracket (openFile fileName ReadMode) hClose hFileSize)
         modTime <- maybeIO (getModificationTime fileName)
         return (FileInfo fileName perms size modTime)

maybeIO :: IO a -> IO (Maybe a)
maybeIO action = handle nothingHandler2 (Just `liftM` action)

nothingHandler2 :: IOException -> IO (Maybe a) -- Exception e => (e -> IO a)
nothingHandler2 _ = return Nothing

traverse :: ([FileInfo] -> [FileInfo]) -> FilePath -> IO [FileInfo]
traverse ordering filePath = do
  fileNames <- getPropperContents filePath
  fullFileNames <- mapM getInfo (filePath : map (filePath </>) fileNames)
  liftM concat $ forM (ordering fullFileNames) $ \thisPath -> do
    if isDirectory thisPath && infoPath thisPath /= filePath
       then traverse ordering (infoPath thisPath)
       else return [thisPath]

getPropperContents :: FilePath -> IO [String]
getPropperContents path = do
  fileNames <- getDirectoryContents path
  return (filter (`notElem` [".",".."]) fileNames)

isDirectory :: FileInfo -> Bool
isDirectory = maybe False searchable . infoPermissions

type FilePredicate = FilePath -> Permissions -> Maybe Integer -> ClockTime -> Bool
type InfoPredicate a = FilePath -> Permissions -> Maybe Integer -> ClockTime -> a

-- lifted operators
(^==) = equalityPredicate
(^&&) = andPredicates
(^||) = orPredicates
(^<) = ltPredicate
(^>) = gtPredicate
infix 4 ^==
infixr 3 ^&&
infixr 3 ^||
infix 4 ^>
infix 4 ^<

nameSizeTest = liftPath takeExtension ^== ".cpp" ^&& sizePredicate ^> 131072

liftPath :: (FilePath -> a) -> InfoPredicate a
liftPath f filePath _ _ _ = f filePath

pathPredicate :: InfoPredicate FilePath
pathPredicate path _ _ _ = path
permissionPredicate :: InfoPredicate Permissions
permissionPredicate _ permission _ _ = permission
sizePredicate :: InfoPredicate Integer
sizePredicate _ _ (Just size) _ = size
sizePredicate _ _ (Nothing) _ = -1
timePredicate :: InfoPredicate ClockTime
timePredicate _ _ _ time = time

liftPredicates :: (a -> b -> c) -> InfoPredicate a -> InfoPredicate b -> InfoPredicate c
liftPredicates comp pred1 pred2 path perm size time = pred1 path perm size time `comp` pred2 path perm size time

andPredicates, orPredicates :: InfoPredicate Bool -> InfoPredicate Bool -> InfoPredicate Bool
andPredicates = liftPredicates (&&)
orPredicates = liftPredicates (||)

equalityPredicate :: (Eq a) => InfoPredicate a -> a -> InfoPredicate Bool
equalityPredicate predicate value path perm size time = predicate path perm size time == value

liftPredicate :: (a -> b -> c) -> InfoPredicate a -> b -> InfoPredicate c
liftPredicate comp predicate value path perm size time = predicate path perm size time `comp` value

gtPredicate, ltPredicate :: (Ord a) => InfoPredicate a -> a -> InfoPredicate Bool
gtPredicate = liftPredicate (>)
ltPredicate = liftPredicate (<)


predicateFind :: FilePredicate -> FilePath -> IO [FilePath]
predicateFind predicate path = getRecursiveFiles path >>= filterM check
  where check fileName = do
         perms <- getPermissions fileName
         size <- getFileSize fileName
         modTime <- getModificationTime fileName
         return (predicate fileName perms size modTime)

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = do
  handle nothingHandler $ do   -- (\_ -> return Nothing)
    bracket (openFile path ReadMode) hClose $ \handle -> do
      fileSize <- hFileSize handle
      return (Just fileSize)
--    fileH <- openFile path ReadMode
--    fileSize <- hFileSize fileH
--    hClose fileH
--    return (Just fileSize)

 -- Control.Exception.IOException
nothingHandler :: IOException -> IO (Maybe Integer) -- Exception e => (e -> IO a)
nothingHandler _ = return Nothing
--alwaysError :: SomeException -> IO String
--alwaysError = \_ -> return "err"

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind test path = do
  allFiles <- getRecursiveFiles path
  return (filter test allFiles)

getRecursiveFiles :: FilePath -> IO [FilePath]
getRecursiveFiles startPath = do
  allContents <- getDirectoryContents startPath
  let properContents = filter (`notElem` [".",".."]) allContents
  allPaths <- forM properContents $ \thisName -> do
--    putStrLn thisName
    let thisPath = startPath </> thisName
    isDirectory <- doesDirectoryExist thisPath
    if isDirectory then getRecursiveFiles thisPath
		   else return [thisPath]
  return (concat allPaths)




{-  startContents <- getDirectoryContents startPath --filter out . ..
  dirsOnly <- filterM (\newDir -> (doesDirectoryExist (startPath++newDir)) ) startContents   -- path seperator blues
  goodDirsOnly <- filterM (\newDir -> ( (return ( not (isPrefixOf "." newDir)))) ) dirsOnly   --  (doesDirectoryExist (startPath++newDir)) &&
  filesOnly <- filterM (doesFileExist.(startPath++)) startContents
  let fullDirPaths = map (startPath++) goodDirsOnly
  putStrLn ("Dirs:"++(show fullDirPaths))
--  putStrLn ("Files:"++(show filesOnly))
--  sequence (map putStrLn startContents)
  sequence (map getRecursiveFiles fullDirPaths)
  return []
-}








{-
System.FilePath.isPathSeparator
System.FilePath.pathSeparators
System.Directory.doesDirectoryExist
System.Directory.doesFileExist
System.Directory.getDirectoryContents



System.FilePath.takeDirectory
  -}




-- Adler crc function from RWH:
base = 65521
adler32_foldl :: [Char] -> Int  -- "Wikipedia" = 300286872
adler32_foldl xs = let (a, b) = foldl step (1, 0) xs
                   in (b `shiftL` 16) .|. a
    where step (a, b) x = let a' = a + (ord x .&. 0xff)
                          in (a' `mod` base, (a' + b) `mod` base)

