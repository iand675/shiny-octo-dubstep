module Setup where
import Control.Monad
import System.Directory
import System.FilePath
import System.Posix.Files

main = do
  linkDotfiles
  linkBinFolder

linkBinFolder :: IO ()
linkBinFolder = do
  bin <- canonicalizePath "./bin"
  home <- getHomeDirectory
  linked <- linkNX bin (home </> "bin")
  if linked
    then putStrLn "Linking ./bin to ~/bin"
    else putStrLn "~/bin already exists"

-- link if not already linked. returns bool for success
linkNX :: FilePath -> FilePath -> IO Bool
linkNX source destination = do
  fileExists <- fileExist destination
  if fileExists
    then return False
    else do
      createSymbolicLink source destination
      return True

linkDotfiles :: IO ()
linkDotfiles = do
  dotfiles <- getDirectoryContents "./dotfiles"
  home <- getHomeDirectory
  let contents = filter (\x -> x /= "." && x /= "..") dotfiles
  mapM_ (linkDotfile home) contents

linkDotfile :: FilePath -> FilePath -> IO ()
linkDotfile home dotfile = do
  let homeLocation = home </> ('.' : dotfile)
      realLocation = "./dotfiles" </> dotfile
  putStr ("Linking " ++ realLocation ++ " to " ++ takeFileName homeLocation ++ "... ")
  symlinkExists <- do
    fileExists <- fileExist homeLocation
    if fileExists
      then do
        status <- getSymbolicLinkStatus homeLocation
        if isSymbolicLink status
          then putStr "already exists " >> return True
          else removeFile homeLocation >> return False
      else return False
  if symlinkExists
    then putStrLn "\ESC[31m✗\ESC[0m"
    else do
      canonicalDotfilePath <- canonicalizePath realLocation
      createSymbolicLink canonicalDotfilePath homeLocation
      putStrLn "\ESC[32m✓\ESC[0m"
