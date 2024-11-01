{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_fp2_haskell (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Projects\\fp2-haskell\\.stack-work\\install\\91663cd9\\bin"
libdir     = "C:\\Projects\\fp2-haskell\\.stack-work\\install\\91663cd9\\lib\\x86_64-windows-ghc-9.6.6\\fp2-haskell-0.1.0.0-DPDcDoYzgiCBUkc2I9GbiI-fp2-haskell-test"
dynlibdir  = "C:\\Projects\\fp2-haskell\\.stack-work\\install\\91663cd9\\lib\\x86_64-windows-ghc-9.6.6"
datadir    = "C:\\Projects\\fp2-haskell\\.stack-work\\install\\91663cd9\\share\\x86_64-windows-ghc-9.6.6\\fp2-haskell-0.1.0.0"
libexecdir = "C:\\Projects\\fp2-haskell\\.stack-work\\install\\91663cd9\\libexec\\x86_64-windows-ghc-9.6.6\\fp2-haskell-0.1.0.0"
sysconfdir = "C:\\Projects\\fp2-haskell\\.stack-work\\install\\91663cd9\\etc"

getBinDir     = catchIO (getEnv "fp2_haskell_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "fp2_haskell_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "fp2_haskell_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "fp2_haskell_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fp2_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fp2_haskell_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
