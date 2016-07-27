module Paths_mssbot (
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
version = Version [0,9,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/lorx/selfihi/mssbot/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/lts-5.9/7.10.3/bin"
libdir     = "/home/lorx/selfihi/mssbot/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/lts-5.9/7.10.3/lib/x86_64-linux-ghc-7.10.3/mssbot-0.9.0-3Ab5sBDJoO15JhfOQnV9sx"
datadir    = "/home/lorx/selfihi/mssbot/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/lts-5.9/7.10.3/share/x86_64-linux-ghc-7.10.3/mssbot-0.9.0"
libexecdir = "/home/lorx/selfihi/mssbot/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/lts-5.9/7.10.3/libexec"
sysconfdir = "/home/lorx/selfihi/mssbot/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/lts-5.9/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mssbot_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mssbot_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "mssbot_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mssbot_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mssbot_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
