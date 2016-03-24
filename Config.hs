module Config
( initConfig
, readConfig
, dropConfigs
) where

import Events

import Data.ConfigFile
import Network.SimpleIRC
import System.Directory
import Control.Monad.Except
import Control.Monad
import Data.List
--import qualified System.IO.UTF8 as I

initConfig :: IO (FilePath,[FilePath])
initConfig = do
    home <- getHomeDirectory
    let configdir = home ++ "/.mssbot"
    exists <- doesDirectoryExist configdir
    unless exists $
      createDirectory configdir
    fileexists <- doesFileExist (configdir++"/default.irc")
    unless fileexists $
      writeFile (configdir++"/default.irc") $ unlines ["network: irc.network.net", "name: botName", "channels = [\"#chan\"]"]
    fullfilelist <- getDirectoryContents configdir
    return (configdir,fullfilelist)

readConfig :: FilePath -> IO IrcConfig
readConfig file = do 
    rv <- runExceptT $ do
        cp <- join $ liftIO $ readfile emptyCP file
        let x = cp
        network <- get x "DEFAULT" "network"
        name <- get x "DEFAULT" "name"
        channels <- get x "DEFAULT" "channels"
        return $ (mkDefaultConfig network name) { cAddr = network
                                                , cNick = name
                                                , cUsername = name
                                                , cRealname = name
                                                , cChannels = read channels ::[String]
                                                , cEvents = events}
    return $ either (\a -> mkDefaultConfig "dead" "dead") id rv

dropConfigs :: [FilePath] -> [FilePath]
dropConfigs [] = []
dropConfigs (f:fs) = if f=="." || f==".." || f=="default.irc" || (head f == '.') || not (".irc" `isInfixOf` f) then dropConfigs fs else f: dropConfigs fs
