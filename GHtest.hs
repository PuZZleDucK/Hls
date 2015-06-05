
module Main where
import System.Environment
import System.Process
import System.IO
import Test.QuickCheck
import Test.QuickCheck.Monadic

main = do
  putStrLn ":: Test Gnu <=> Haskell implementations ::"
  (arg:_) <- getArgs
  putStrLn $ "Testing: " ++ (show arg)
  putStrLn $ "Gnu cmd: " ++ arg ++ "  Haskell cmd: H" ++ arg ++".hs"
  outGnu <- runGnuCmd arg "y"
  putStrLn $ "5 Gnu:\n" ++ unlines (take 5 outGnu)
  outHas <- runHasCmd arg "y"
  putStrLn $ "5 Has:\n" ++ unlines (take 5 outHas)
  quickCheck (ioPropOne arg)
  return ()


--ioPropOne :: String -> ioProperty
ioPropOne cmd args =  monadicIO $ do
  g <- run (runGnuCmd cmd ("--\""++args++"\""))
  h <- run (runHasCmd cmd ("--\""++args++"\""))
  assert (take 100 g == take 100 h)

runGnuCmd :: String -> String -> IO [String]
runGnuCmd cmd args = do
  (_i, o, _e, _hand) <- runInteractiveCommand ("/usr/bin/"++cmd++" "++args)
  hSetBinaryMode o True
  output <- hGetContents o
  return (lines output)

runHasCmd :: String -> String -> IO [String]
runHasCmd cmd args = do
  (_i, o, _e, _hand) <- runInteractiveCommand ("/home/puzzleduck/Dropbox/x/haskell/files/hls/H"++cmd++" "++args)
  hSetBinaryMode o True
  output <- hGetContents o
  return (lines output)


