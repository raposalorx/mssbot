module IO
( address
, send
, runCmd
, getTitleFile
, getTitle
, getRedirectTitle
, saveTell
, tell
, roll
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
import Control.Monad
import Control.Exception
import System.IO.Error
import Data.List
import Data.Time
import System.Random

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
  (exit,out,err) <- readProcessWithExitCode cmd args stdin
  print exit
  return $ if exit==ExitSuccess then out else "Command Failed"

getTitleFile chan = do
  home <- getHomeDirectory
  return $ home++"/.mssbot/"++chan++"title"

getUrlFile = do
  home <- getHomeDirectory
  return $ home++"/.mssbot/urltmp"

getGoogleFile = do
  home <- getHomeDirectory
  return $ home++"/.mssbot/googletmp"

getTellFile = do
  home <- getHomeDirectory
  return $ home++"/.mssbot/telllist"

download :: String -> String -> IO ()
download url file = void $ runCmd "curl" ["-sSL", "-m", "10", "--user-agent","Mozilla/4.0", "-o",file, url] ""

getRedirectTitle :: String -> IO String
getRedirectTitle search = do
  googleFile <- getGoogleFile
  download search googleFile
  url <- flip stringRegex "(?<=\"url\":\")[^\"]*" <$> I.readFile googleFile
  title <- getTitle url
  return $ concat [url, " -- ", title]

getTitle :: String -> IO String
getTitle url = do
  urlFile <- getUrlFile
  let patchedUrl = if not . null $ stringRegex url "(http(s)?://)?(www.)?youtube.com(/)?[^ ()[\\]`'\"]*" then url++"&gl=CA&hl=en" else url
--    putStrLn $ concat ["Getting ", patchedUrl] 
  download patchedUrl urlFile
  usable <- flip elem ["HTML", "xHTML", "XML"] <$> takeWhile (/=' ') <$> runCmd "file" ["-b", urlFile] ""
--    putStrLn $ concat ["It's a ", show usable, " document"]
  if usable then do
      html <- I.readFile urlFile
      let title = matchTitle html
      return $ if not . null $ title then title else ""
      else return ""

tell :: MIrc -> String -> IO()
tell s nik = do
    tfile <- getTellFile
    e <- tryJust (guard . isDoesNotExistError) (I.readFile tfile)
    let messages = map (\a -> read a :: (String, String, String, String)) . lines . either (const "") id $ e
    goTell s nik . map (\(a,b,c,t) -> concat [t, " UTC <",b,"> tell ",nik," ",c]) . matchnik $ messages
    I.writeFile tfile . unlines . map show . matchnik $ messages
  where
      matchnik = filter (\(a,b,c,t) -> isPrefixOf (lower a) (lower nik))
      goTell _ _ [] = return ()
      goTell s nik (t:ts) = do
        sendRaw s . U.fromString . concat $ ["PRIVMSG ", nik, " :", t]
        goTell s nik ts

saveTell :: U.ByteString -> String -> IO String
saveTell msg from = do
  let (mnick, message) = span (/=' ') $ stringDropCmd msg
  time <- flip stringRegex "[^\\.]*(?=:[0-9]{2}\\.)" . show <$> getCurrentTime
  tfile <- getTellFile
  I.appendFile tfile $ show (mnick, from, dropWhile (==' ') message, time) ++ "\n"
  return "I'll totally pass that on for you!"

roll :: Int -> IO Int
roll d = getStdRandom (randomR (1,d))
