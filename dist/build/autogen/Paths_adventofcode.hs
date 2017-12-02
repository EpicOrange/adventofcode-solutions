{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_adventofcode (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/daniel/.cabal/bin"
libdir     = "/Users/daniel/.cabal/lib/x86_64-osx-ghc-8.0.2/adventofcode-0.1.0.0-L98YYB23kHdFT6MRQt8cy9"
dynlibdir  = "/Users/daniel/.cabal/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/daniel/.cabal/share/x86_64-osx-ghc-8.0.2/adventofcode-0.1.0.0"
libexecdir = "/Users/daniel/.cabal/libexec"
sysconfdir = "/Users/daniel/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "adventofcode_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "adventofcode_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "adventofcode_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "adventofcode_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "adventofcode_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "adventofcode_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
