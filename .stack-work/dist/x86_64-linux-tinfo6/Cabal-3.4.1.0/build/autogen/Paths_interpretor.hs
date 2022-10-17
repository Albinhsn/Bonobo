{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_interpretor (
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

bindir     = "/home/arlaharen/dev/interpretor/.stack-work/install/x86_64-linux-tinfo6/f2bfd0e764dc484fa379b51d6feb9f76a38377ffef8e05f31ead72c87916f08b/9.0.2/bin"
libdir     = "/home/arlaharen/dev/interpretor/.stack-work/install/x86_64-linux-tinfo6/f2bfd0e764dc484fa379b51d6feb9f76a38377ffef8e05f31ead72c87916f08b/9.0.2/lib/x86_64-linux-ghc-9.0.2/interpretor-0.1.0.0-CIEEk7fBgZ6FgXjLhe3j8t"
dynlibdir  = "/home/arlaharen/dev/interpretor/.stack-work/install/x86_64-linux-tinfo6/f2bfd0e764dc484fa379b51d6feb9f76a38377ffef8e05f31ead72c87916f08b/9.0.2/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/arlaharen/dev/interpretor/.stack-work/install/x86_64-linux-tinfo6/f2bfd0e764dc484fa379b51d6feb9f76a38377ffef8e05f31ead72c87916f08b/9.0.2/share/x86_64-linux-ghc-9.0.2/interpretor-0.1.0.0"
libexecdir = "/home/arlaharen/dev/interpretor/.stack-work/install/x86_64-linux-tinfo6/f2bfd0e764dc484fa379b51d6feb9f76a38377ffef8e05f31ead72c87916f08b/9.0.2/libexec/x86_64-linux-ghc-9.0.2/interpretor-0.1.0.0"
sysconfdir = "/home/arlaharen/dev/interpretor/.stack-work/install/x86_64-linux-tinfo6/f2bfd0e764dc484fa379b51d6feb9f76a38377ffef8e05f31ead72c87916f08b/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "interpretor_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "interpretor_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "interpretor_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "interpretor_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "interpretor_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "interpretor_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
