module Paths_OrpheusStundenplan (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/sven/.cabal/bin"
libdir     = "/home/sven/.cabal/lib/x86_64-linux-ghc-7.10.3/OrpheusStundenplan-0.1.0.0-9kquiHoN81g5GQ4PkYlIpp"
datadir    = "/home/sven/.cabal/share/x86_64-linux-ghc-7.10.3/OrpheusStundenplan-0.1.0.0"
libexecdir = "/home/sven/.cabal/libexec"
sysconfdir = "/home/sven/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "OrpheusStundenplan_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "OrpheusStundenplan_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "OrpheusStundenplan_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "OrpheusStundenplan_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "OrpheusStundenplan_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
