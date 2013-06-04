module Config
( initConfig
, readConfig
, dropConfigs
) where

import Events

import Data.ConfigFile
import Network.SimpleIRC
import System.Directory
import Control.Monad.Error
import Data.List
import qualified System.IO.UTF8 as I

initConfig :: IO (FilePath,[FilePath])
initConfig = do
    home <- getHomeDirectory
    let configdir = home ++ "/.mssbot"
    exists <- doesDirectoryExist configdir
    if not exists then createDirectory configdir else return ()
    fileexists <- doesFileExist (configdir++"/default.irc")	
    if not fileexists then I.writeFile (configdir++"/default.irc") $ unlines ["network: irc.network.net", "name: botName", "channels = [\"#chan\"]"] else return ()
    fullfilelist <- getDirectoryContents configdir
    return (configdir,fullfilelist)

readConfig :: FilePath -> IO IrcConfig
readConfig file = do 
    rv <- runErrorT $ do
        cp <- join $ liftIO $ readfile emptyCP file
        let x = cp
        network <- get x "DEFAULT" "network"
        name <- get x "DEFAULT" "name"
        channels <- get x "DEFAULT" "channels"
        return $ (mkDefaultConfig network name) {cAddr = network, cNick = name, cUsername = name, cRealname = name, cChannels = (read channels ::[String]), cEvents = events}
    return $ either (\a -> mkDefaultConfig "dead" "dead") (\b -> b) rv

dropConfigs :: [FilePath] -> [FilePath]
dropConfigs [] = []
dropConfigs (f:fs) = if f=="." || f==".." || f=="default.irc" || (head f == '.') || (not $ isInfixOf ".irc" f) then dropConfigs fs else f: dropConfigs fs
