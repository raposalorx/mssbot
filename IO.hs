module IO
( address
, send
, runCmd
, getTitleFile
, getTitle
) where

import Text

import Network.SimpleIRC
import Data.Maybe
import System.Process
import System.Exit
import System.Directory
import qualified Data.ByteString.UTF8 as U
import qualified System.IO.UTF8 as I
import Control.Applicative

address :: String -> String -> String
address nik s = concat [nik, ": ", s]

send :: MIrc -> IrcMessage -> String -> IO()
send s m msg = do
    let chan = fromJust $ mChan m
    nick <- getNickname s
    let from = fromJust $ mNick m
    let to = U.toString $ if nick == chan then from else chan
    sendRaw s $ U.fromString $ concat ["PRIVMSG ", to, " :", msg]

runCmd :: String -> [String] -> String -> IO String
runCmd cmd args stdin= do
    (exit,out,err) <- readProcessWithExitCode cmd (args) stdin
    putStrLn $ show exit
    if exit==ExitSuccess then return out else return "Command Failed"

getTitleFile chan = do
    home <- getHomeDirectory
    return $ home++"/.mssbot/"++chan++"title"

getUrlFile = do
    home <- getHomeDirectory
    return $ home++"/.mssbot/urltmp"

download :: String -> String -> IO ()
download url file = runCmd "curl" ["-sSL", "-m", "10", "--user-agent","Mozilla/4.0", "-o",file, url] "" >> return ()

getTitle :: String -> IO String
getTitle url = do
    urlFile <- getUrlFile
    let patchedUrl = if (length $ stringRegex url "(http(s)?://)?(www.)?youtube.com(/)?[^ ()[\\]`'\"]*") > 0 then url++"&gl=CA&hl=en" else url
--    putStrLn $ concat ["Getting ", patchedUrl] 
    download patchedUrl urlFile
    usable <- flip elem ["HTML", "xHTML", "XML"] <$> takeWhile (/=' ') <$> runCmd "file" ["-b", urlFile] ""
--    putStrLn $ concat ["It's a ", ftype, " document"]
    if usable then do
        html <- I.readFile urlFile
        let title = matchTitle html
        if length title > 0 then return title else return ""
        else return ""

