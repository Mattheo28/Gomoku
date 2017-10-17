module Paths_Gomoku (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/cs/home/mi42/.cabal/bin"
libdir     = "/cs/home/mi42/.cabal/lib/x86_64-linux-ghc-7.8.4/Gomoku-0.1.0.0"
datadir    = "/cs/home/mi42/.cabal/share/x86_64-linux-ghc-7.8.4/Gomoku-0.1.0.0"
libexecdir = "/cs/home/mi42/.cabal/libexec"
sysconfdir = "/cs/home/mi42/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Gomoku_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Gomoku_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Gomoku_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Gomoku_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Gomoku_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
