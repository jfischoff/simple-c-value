module Paths_simple_c_value (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/jonathan.fischoff/Library/Haskell/ghc-7.4.1/lib/simple-c-value-0.0.0.1/bin"
libdir     = "/Users/jonathan.fischoff/Library/Haskell/ghc-7.4.1/lib/simple-c-value-0.0.0.1/lib"
datadir    = "/Users/jonathan.fischoff/Library/Haskell/ghc-7.4.1/lib/simple-c-value-0.0.0.1/share"
libexecdir = "/Users/jonathan.fischoff/Library/Haskell/ghc-7.4.1/lib/simple-c-value-0.0.0.1/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "simple_c_value_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "simple_c_value_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "simple_c_value_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "simple_c_value_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
