{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Data.Maybe
import Data.Time
import Data.String
import Data.List
import Data.Char
import GHC.Exts( IsString(..) )
import Text.Regex
import Text.Regex.PCRE
import Text.Regex.PCRE.String
import Data.Functor
import Network.HTTP.Wget
import System.IO
import System.Exit
import System.Process
import System.Random
import System.Directory
import Language.Translate.Google
import Web.Encodings
import Network.MPD
import Data.ConfigFile
import Control.Monad.Error
import Control.Concurrent.MVar
import Control.Concurrent
import System.Posix.Unistd
import qualified System.IO.UTF8 as I
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as U

helpstr = "Commands (prefix ?): h [command] (help), tell <nick> <message>, ping [url], dc <equation>, eval <expression>, t <string>, g <query>, wik <query>, tube <query>, weather <location>[,province[,country]], d <[x|]<y>d<z>[+/-w]>..., tatl #; Passive: Report titles for urls;"

help s m "h"		= send s m "?h [command] - A help dialog for command, Or a list of commands."
help s m "tell"		= send s m "?tell <nick> <message> - Send \"<nick> tell message\" as a PM to nick next time they speak."
help s m "ping"		= send s m "?ping [url] - Ping a site and return it's response time. Or just pong the user."
help s m "dc"       = send s m "?dc <equation> - Arbitrary precision reverse polish calculator."
help s m "eval"     = send s m "?eval <expression> - Haskell expression"
help s m "t"		= send s m "?t <string> - Translate string to English using google translate."
help s m "g"		= send s m "?g <query> - Return the first google search result matching query."
help s m "wik"		= send s m "?wik <query> - Return the first wikipedia search result matching query."
help s m "tube"		= send s m "?tube <query> - Return the first youtube search result matching query."
help s m "weather"	= send s m "?weather <location>[,province[,country]] - Get the weather from location."
help s m "d"		= send s m "?d <[x|]<y>d<z>[+/-w]>... - Sum of the values of y dice with z sides, plus or minus w, x times."
help s m "tatl"     = send s m "?tatl # - Link the sentance numbered # from tatoeba.org"
help s m _          = send s m helpstr

onMessage :: EventFunc
onMessage s m
    | msg == "?h" = send s m helpstr
    | B.isPrefixOf "?h " msg = help s m $ takeWhile (/=' ') $ stringDropCmd msg
    | msg == "?ping" = send s m $ address nick "pong!"
    | B.isPrefixOf "?ping " msg = do
        let url = flip stringRegex "(www.)?([a-zA-Z0-9\\-_]{1,}\\.){1,}[a-zA-Z]{2,4}(/)?[^ ]*" $ takeWhile (/=' ') $ stringDropCmd msg
        if length url > 0 then do
            ping <- runCmd "ping" ["-c 2 -w 3", url] ""
            let time = stringRegex ping "(?<=time=)[0-9]*"
            send s m $ address nick $ length time > 0 ? concat [time, "ms"] $ "Can't connect."
            else send s m $ address nick "pong!"
    | B.isPrefixOf "?eval " msg = do
        let eq = stringDropCmd msg
        out <- runCmd "mueval" ["-m","Text.Regex.PCRE","-e", eq] ""
        putStrLn out
        send s m $ address nick $ out
    | B.isPrefixOf "?dc " msg = do
        let eq = map (\x -> if x=='!' then ' ' else x) $ stringDropCmd msg
        out <- runCmd "dc" [] (eq++" n")
        send s m $ address nick $ unwords.lines $ out
    | msg == "?mt" = do
        cur <- withMPD $ currentSong
        stat <- withMPD $ status
        let curr = show cur
        let stats = show stat
        let song = stringRegex curr "(?<=Title,\\[\")[^\"]*"
        let artist = stringRegex curr "(?<=Artist,\\[\")[^\"]*"
        let state = stringRegex stats "(?<=stState = )[^,]*"
        let times = stringRegex stats "(?<=stTime = \\()[^\\)]*"
        let curtime = time $ read $ stringRegex times "[^\\.]*"
        let lentime = time $ read $ stringRegex times "(?<=,).*"
        send s m $ state == "Stopped" ? state $ concat [state, ": ", song, " - ", artist, " [", curtime, "/", lentime, "]"]
    | B.isPrefixOf "?tell " msg = do
        let (mnick, message) = span (/=' ') $ stringDropCmd msg
--        if all (\a -> any (\b -> b==a) (['_']++['a'..'z']++['A'..'Z'])) mnick then do
        ftime <- getCurrentTime
        let time = stringRegex (show ftime) "[^\\.]*(?=:[0-9]{2}\\.)"
        tellFile <- getTellFile
        I.appendFile tellFile $ concat [show (mnick, nick, concat [dropWhile (==' ') message], time),"\n"]
        send s m "I'll totally pass that on for you!"
--            else send s m "That's not a valid nick, try again."
{-    | B.isPrefixOf "?all " msg = do
        sendRaw s $ U.fromString $ concat ["NAMES #jbopre"]
-}{-  | B.isPrefixOf "?remind " msg = do
    let (mnick, mmessage) = span (/=' ') $ stringDropCmd msg
        putStrLn mmessage
        let (attime, message) = span (/=' ') $ dropWhile (==' ') mmessage
        putStrLn message
        ftime <- getCurrentTime
        let fnewtime = findRemindTime attime ftime
        putStrLn fnewtime
        let newtime = stringRegex fnewtime ".*(?=:[0-9]{2} UTC)"
        remindFile <- getRemindFile
        I.appendFile remindFile $ concat [show (mnick, dropWhile (==' ') message, newtime),"\n"]
        send s m $ concat ["Cool, I'll remind you on ", newtime]-}
    | B.isPrefixOf "?t " msg = do
        trans <- translate (dropCommand msg) Nothing English
        send s m $ address nick $ decodeHtml $ U.toString $ either (\e -> U.fromString e) (\r -> r) trans
    | B.isPrefixOf "?d " msg = do
        ds <- collapseroll $ map (droll) $ map (\a -> (dieD a, dieMulti a, dieOffset a, dieLoop a)) $ map (head) $ listRegex (stringDropCmd msg) "([0-9]?\\|)?([0-9]+)?d([0-9]+|%)((\\+|-)[0-9]+)?"
        send s m $ address nick ds
    | B.isPrefixOf "?g " msg = do
        let search = concat ["http://ajax.googleapis.com/ajax/services/search/web?v=1.0&safe=off&q=", spaceToPlus $ stringDropCmd msg]
        googleFile <- getGoogleFile
        download search googleFile
        redir <- I.readFile googleFile
        let url = stringRegex redir "(?<=\"url\":\")[^\"]*"
        title <- getTitle url
        send s m $ address nick $ concat [url, " -- ", title]
    | B.isPrefixOf "?wik " msg = do
        let search = concat ["http://ajax.googleapis.com/ajax/services/search/web?v=1.0&safe=off&q=site%3Awww.wikipedia.com+", spaceToPlus $ stringDropCmd msg]
        googleFile <- getGoogleFile
        download search googleFile
        redir <- I.readFile googleFile
        let url = stringRegex redir "(?<=\"url\":\")[^\"]*"
        title <- getTitle url
        send s m $ address nick $ concat [url, " -- ", title]
    | B.isPrefixOf "?tube " msg = do
        let search = concat ["http://ajax.googleapis.com/ajax/services/search/web?v=1.0&safe=off&q=sit%3Awww.youtube.com+", spaceToPlus $ stringDropCmd msg]
        googleFile <- getGoogleFile
        download search googleFile
        redir <- I.readFile googleFile
        let url = stringRegex redir "(?<=\"url\":\")[^\"]*"
        title <- getTitle url
        send s m $ address nick $ concat [url, " -- ", title]
    | B.isPrefixOf "?weather " msg = do
        let search = concat ["http://www.google.com/ig/api?weather=", spaceToPlus $ stringDropCmd msg]
        googleFile <- getGoogleFile
        download search googleFile
        redir <- I.readFile googleFile
        if boolRegex redir "city data" then do
            let his = map (\a -> read a ::Int) $ map (head) $ listRegex redir "(?<=<high data=\")[^\"]*"
            let lows = map (\a -> read a ::Int) $ map (head) $ listRegex redir "(?<=<low data=\")[^\"]*"
            let conditions = map (head) $ listRegex redir "(?<=<condition data=\")[^\"]*"
            let form = concat [stringRegex redir "(?<=<city data=\")[^\"]*", ": ", stringRegex redir "(?<=<temp_c data=\")[^\"]*", "C ", show $ fToC $ his!!0, "H ", show $ fToC $ lows!!0, "L ", stringRegex redir "(?<=<wind_condition data=\")[^\"]*"," and ", conditions!!0, ", expect ", conditions!!1]
            send s m $ address nick form
            else putStrLn search
    | B.isPrefixOf "?tatl " msg = do
        let url = concat ["http://tatoeba.org/eng/sentences/show/", stringDropCmd msg]
        title <- getTitle url
        send s m $ address nick $ concat ["\"", stringRegex title "(?<=sentence: ).*$", "\" ( ",url," )"]
    | otherwise = do
        let message = B.unpack msg
        let url = stringRegex message "(http(s)?://)(www.)?([a-zA-Z0-9\\-_]{1,}\\.){1,}[a-zA-Z]{2,4}(/)?[^ ()[\\]`'\"]*"
        tell s m nick
        myNick <- getNickname s
        if B.isInfixOf myNick (U.fromString $ lower $ U.toString msg) then do
            let (sal,check) = span (/=' ') $ U.toString msg
            if (noSpaces $ noPunc $ lower check) == (U.toString myNick) then do
                send s m $ concat [sal, " ", nick]
                else return () else return ()
        if length url > 0 then do
            title <- getTitle url
            send s m $ decodeHtml title
            else return () -- putStrLn $ concat [U.toString chan, " -> ", "< ", nick, "> ", U.toString msg]
    --    putStrLn $ show m
    where chan = fromJust $ mChan m
          msg = mMsg m
          nik = fromJust $ mNick m
          nick = U.toString nik

getTitle :: String -> IO String
getTitle url = do
    putStrLn $ concat ["Getting ", url] 
    urlFile <- getUrlFile
    download url urlFile
    kindoffile <- runCmd "file" ["-b", urlFile] ""
    let ftype = takeWhile (/=' ') kindoffile
    putStrLn $ concat ["It's a ", ftype, " document"]
    if ftype == "HTML" || ftype == "xHTML" || ftype == "XML" then do
        html <- I.readFile urlFile
        let title0 =  killSpaces $ map (\x -> if x=='\r' then ' ' else x) $ flip stringRegex "(?<=>)[^<]*" $ stringRegex (unwords.lines $ html)  "<[^>]*[tT][iI][tT][lL][eE][^>]*>[^<]*<[^>]*/[^>]*[tT][iI][tT][lL][eE][^>]*>"
        let title =  if (length $ stringRegex title0 "&rlm;.$") > 0 then reverse $ drop 6 $ reverse title0 else title0
        length title > 0 ? return title $ return ""
        else return ""

tell :: MIrc -> IrcMessage -> String -> IO()
tell s m nik = do
    tellFile <- getTellFile
    isTellFile <- doesFileExist tellFile
    if isTellFile then do
        all <- I.readFile tellFile
        let messages = map (\a -> read a :: (String, String, String, String)) $ lines all
        let tells = map (\(a,b,c,t) -> concat $ [t, " UTC <",b,"> tell ",nik," ",c]) $ filter (\(a,b,c,t) -> isPrefixOf (lower a) (lower nik)) messages
        goTell s m tells
        I.writeFile tellFile $ unlines $ map (show) $ filter (\(a,b,c,t) -> not (isPrefixOf (lower a) (lower nik))) messages
        else I.writeFile tellFile ""
    where
        goTell _ _ [] = return ()
        goTell s m (t:ts) = do
        sendRaw s $ U.fromString $ concat ["PRIVMSG ", U.toString $ fromJust $ mNick m, " :", t]
        goTell s m ts

{-findRemindTime :: String -> DateTime -> String
findRemindTime at cur = show $ addMinutes (days*24*60) $ addMinutes (hours*60) $ addMinutes minutes $ addSeconds seconds cur
where
    seconds = read $ (\a -> if a=="" then "0" else a) $ stringRegex at "[0-9]+(?=s)" ::Integer
    minutes = read $ (\a -> if a=="" then "0" else a) $ stringRegex at "[0-9]+(?=m)" ::Integer
    hours = read $ (\a -> if a=="" then "0" else a) $ stringRegex at "[0-9]+(?=h)" ::Integer
    days = read $ (\a -> if a=="" then "0" else a) $ stringRegex at "[0-9]+(?=d)" ::Integer-}

send :: MIrc -> IrcMessage -> String -> IO()
send s m msg = do
    let chan = fromJust $ mChan m
    nick <- getNickname s
    let from = fromJust $ mNick m
    let to = if nick == chan then from else chan
    sendRaw s $ U.fromString $ concat ["PRIVMSG ", U.toString to, " :", msg]

runCmd :: String -> [String] -> String -> IO String
runCmd cmd args stdin = do
    (exit,out,err) <- readProcessWithExitCode cmd args stdin
    putStrLn $ show exit
    if exit==ExitSuccess then return out else return "Command Failed"

killSpaces :: String -> String
killSpaces [] = ""
killSpaces (a:[]) = a=='\t' || a=='\n' ? [] $ [a]
killSpaces (a:b:ss) = ((a == ' ' && b == ' ') || a=='\t' || a=='\n' ? (\z->z) $ (a:)) $ killSpaces $ b:ss

download :: String -> String -> IO()
download url file = runCmd "curl" ["-sSL", "-m", "10", "--user-agent","Mozilla/4.0", "-o",file, url] "" >> return ()
    --if success == "Command Failed" then  I.writeFile file "<title>Can't get Title</title>" else return ()

droll :: (Int, Int, Int, Int) -> IO [Int]
droll (_, _, _, 0) = return []
droll (d, multi, offset, num) = do
    m <- (dmulti d multi)
    m2 <- droll (d, multi, offset, num-1)
    return $ (m+offset):m2
    where
        dmulti _ 0 = return 0
        dmulti dm mul = do
            r <- roll d
            r2 <- dmulti dm (mul-1)
            return $ r+r2

dieMulti :: String -> Int
dieMulti a = length b == 0 ? 1 $ read b ::Int
    where b = stringRegex a "([0-9]+)?(?=d)"

dieOffset :: String -> Int
dieOffset a = length b == 0 ? 0 $ if head b == '+' then read $ drop 1 b ::Int else read b ::Int
    where b = stringRegex a "(\\+|-)[0-9]+"

dieLoop :: String -> Int
dieLoop a = length b == 0 ? 1 $ read b ::Int
    where b = stringRegex a "[0-9]+(?=\\|)"

dieD :: String -> Int
dieD a = b == "%" ? 100 $ read b ::Int
    where b = stringRegex a "(?<=d)([0-9]+|%)"

roll :: Int -> IO Int
roll d = getStdRandom (randomR (1,d))

collapseroll :: [IO [Int]] -> IO String
collapseroll [] = return ""
collapseroll (i:is) = do
    a <- i
    b <- collapseroll is
    return $ unwords [(unwords $ map (show) a),b]

fToC :: Int -> Int
fToC f = round $ (*) (5 / 9) $ flip (-) 32 $ fromIntegral f

dropCommand :: B.ByteString -> B.ByteString
dropCommand b = B.drop 1 $ B.dropWhile (/= ' ') b

stringDropCmd :: B.ByteString -> String
stringDropCmd = U.toString . dropCommand

stringRegex :: String -> String -> String
stringRegex orig regex = orig =~ regex

listRegex :: String -> String -> [[String]]
listRegex orig regex = orig =~ regex :: [[String]]

--compRegex :: String -> IO Text.Regex.PCRE.String.Regex
--compRegex s = either (\_ -> makeRegex "") (\x -> x) <$> compile compUTF8 0 s

evalRegex :: Text.Regex.PCRE.String.Regex -> String -> IO (String, String, String, [String])
evalRegex reg s = either (\_ -> ("","","",[""])) (\x -> maybe ("","","",[""]) (\y -> y) x) <$> regexec reg s

pullResult :: (String, String, String, [String]) -> String
pullResult (_,x,_,_) = x

matchRegex :: Text.Regex.PCRE.String.Regex -> String -> IO String
matchRegex reg s = pullResult <$> evalRegex reg s

boolRegex :: String -> String -> Bool
boolRegex orig regex = orig =~ regex :: Bool

spaceToPlus :: String -> String
spaceToPlus = map (\a -> a==' ' ? '+' $ a)

noSpaces :: String -> String
noSpaces = foldr (\a b -> a==' ' ? b $ a:b) ""

noPunc :: String -> String
noPunc = foldr (\a b -> a=='.' || a==',' || a=='!' ? b $ a:b) ""

address :: String -> String -> String
address nik s = concat [nik, ": ", s]

secondBuffer :: String -> String
secondBuffer [] = "00"
secondBuffer (s:[]) = '0':s:""
secondBuffer s = s

time :: Int -> String
time i = minutes == 0 && seconds == 0 ? "--:--" $ concat [show minutes, ":", secondBuffer $ show seconds]
    where
        minutes = floor $ flip (/) 60 $ fromIntegral i
        seconds = i - (minutes*60)

lower :: String -> String
lower = map (toLower)

capitalize :: String -> String
capitalize (s:ss) = (toUpper s):ss

{-onDisconnect mIrc = do
    m <- reconnect mIrc
    either (\err -> putStrLn $ "Unable to reconnect: " ++ show err) (\_ -> putStrLn "Successfully reconnected.") m
-}
onRaw s m = putStrLn $ show m

events = [(Privmsg onMessage){-,(Disconnect onDisconnect)-},(RawMsg onRaw)]

getTellFile = do
    home <- getHomeDirectory
    return $ home++"/.mssbot/telllist"
getGoogleFile = do
    home <- getHomeDirectory
    return $ home++"/.mssbot/googletmp"
getUrlFile = do
    home <- getHomeDirectory
    return $ home++"/.mssbot/urltmp"
getRemindFile = do
    home <- getHomeDirectory
    return $ home++"/.mssbot/remindlist"

-- ternary
infixr 1 ?
True ? x = const x
False ? _ = id

initConfig :: IO FilePath
initConfig = do
    home <- getHomeDirectory
    let configdir = home ++ "/.mssbot"
    exists <- doesDirectoryExist configdir
    not exists ? createDirectory configdir $ return ()
    fileexists <- doesFileExist (configdir++"/default.irc")	
    if not fileexists then I.writeFile (configdir++"/default.irc") $ unlines ["network: irc.network.net", "name: botName", "channels = [\"#chan\"]"] else return ()
    return configdir

readConfig :: FilePath -> IO IrcConfig
readConfig file = do 
    rv <- runErrorT $ do
        cp <- join $ liftIO $ readfile emptyCP file
        let x = cp
        network <- get x "DEFAULT" "network"
        name <- get x "DEFAULT" "name"
        channels <- get x "DEFAULT" "channels"
        return defaultConfig {cAddr = network, cNick = name, cUsername = name, cRealname = name, cChannels = (read channels ::[String]), cEvents = events}
    return $ either (\a -> defaultConfig) (\b -> b) rv

dropConfigs :: [FilePath] -> [FilePath]
dropConfigs [] = []
dropConfigs (f:fs) = if f=="." || f==".." || f=="default.irc" || (not $ isInfixOf ".irc" f) then dropConfigs fs else f: dropConfigs fs

main = do
    configdir <- initConfig
    fullfilelist <- getDirectoryContents configdir
    let files = map ((configdir++"/")++) $ dropConfigs fullfilelist
    putStr "Opening: "
    putStrLn $ show configdir
    putStr "Connecting to: "
    putStrLn $ show files
    configs <- mapM (readConfig) files
    mapM (\net -> connect net True False) $ drop 1 configs
    connect (configs!!0) False True

