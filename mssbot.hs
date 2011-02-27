{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Data.Maybe
import Data.String
import Data.List
import Data.Time
import Data.Char
import GHC.Exts( IsString(..) )
import Text.Regex.PCRE
import Network.HTTP.Wget
import System.IO
import System.Exit
import System.Process
import System.Random
import System.Directory
import Language.Translate.Google
import Web.Encodings
import Network.MPD
import qualified System.IO.UTF8 as I
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as U

freenode = defaultConfig
  { cAddr = "irc.freenode.net"
  , cNick = "mssbotdev"
  , cChannels = ["##mssdev", "#maelstrom"]
  , cEvents = events
  }

tellFile = "/var/tmp/mssbottelllist"
googleFile = "/var/tmp/mssbotgoogle"
urlFile = "/var/tmp/mssboturl"

onMessage :: EventFunc
onMessage s m
  | msg == "?h" = do
    sendMsg s chan "Commands (prefix ?): h (help), tell <nick> <message>, ping, t <string> (translate), g <query> (google), wik <query>, weather <location>[,province], d <[x|]<y>d<z>[+/-w]>... (dice); Passive: Report titles for urls;"
  | msg == "?ping" = sendMsg s chan $ address nick "pong!"
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
		if state == "Stopped" then sendMsg s chan $ U.fromString state else sendMsg s chan $ U.fromString $ concat [state, ": ", song, " - ", artist, " [", curtime, "/", lentime, "]"]
  | B.isPrefixOf "?tell " msg = do
		let (mnick, message) = span (/=' ') $ stringDropCmd msg
		ftime <- getCurrentTime
		let time = stringRegex (show ftime) "[^\\.]*(?=:[0-9]{2}\\.)"
		I.appendFile tellFile $ concat [show (mnick, nick, concat [dropWhile (==' ') message], time),"\n"]
		sendMsg s chan $ U.fromString "I'll totally pass that on for you!"
  | B.isPrefixOf "?t " msg = do
		trans <- translate (dropCommand msg) Nothing English
		sendMsg s chan $ address nick $ decodeHtml $ U.toString $ either (\e -> U.fromString e) (\r -> r) trans
  | B.isPrefixOf "?d " msg = do
		ds <- collapseroll $ map (droll) $ map (\a -> (dieD a, dieMulti a, dieOffset a, dieLoop a)) $ map (head) $ listRegex (stringDropCmd msg) "([0-9]?\\|)?([0-9]+)?d([0-9]+|%)((\\+|-)[0-9]+)?"
		sendMsg s chan $ address nick ds
  | B.isPrefixOf "?g " msg = do
		let search = concat ["http://ajax.googleapis.com/ajax/services/search/web?v=1.0&safe=off&q=", spaceToPlus $ stringDropCmd msg]
		download search googleFile
		redir <- I.readFile googleFile
		sendMsg s chan $ address nick $ stringRegex redir "(?<=\"url\":\")[^\"]*"
  | B.isPrefixOf "?wik " msg = do
		let search = concat ["http://ajax.googleapis.com/ajax/services/search/web?v=1.0&safe=off&q=%3Asite+www.wikipedia.com+", spaceToPlus $ stringDropCmd msg]
		download search googleFile
		redir <- I.readFile googleFile
		sendMsg s chan $ address nick $ stringRegex redir "(?<=\"url\":\")[^\"]*"
  | B.isPrefixOf "?weather " msg = do
		let search = concat ["http://www.google.com/ig/api?weather=", spaceToPlus $ stringDropCmd msg]
		download search googleFile
		redir <- I.readFile googleFile
		if boolRegex redir "city data" then do
		let his = map (\a -> read a ::Int) $ map (head) $ listRegex redir "(?<=<high data=\")[^\"]*"
		let lows = map (\a -> read a ::Int) $ map (head) $ listRegex redir "(?<=<low data=\")[^\"]*"
		let conditions = map (head) $ listRegex redir "(?<=<condition data=\")[^\"]*"
		let form = concat [stringRegex redir "(?<=<city data=\")[^\"]*", ": ", stringRegex redir "(?<=<temp_c data=\")[^\"]*", "C ", show $ fToC $ his!!0, "H ", show $ fToC $ lows!!0, "L ", stringRegex redir "(?<=<wind_condition data=\")[^\"]*"," and ", conditions!!0, ", expect ", conditions!!1]
		sendMsg s chan $ address nick form
		else putStrLn search
  | otherwise = do
		let message = B.unpack msg
		let url = stringRegex message "(http(s)?://)(www.)?([a-zA-Z0-9\\-_]{1,}\\.){1,}[a-zA-Z]{2,4}(/)?[^ ]*"
		tell s chan nick
		if length url > 0 then do
		title <- getTitle url
		sendMsg s chan $ U.fromString $ decodeHtml title
		else putStrLn $ show m
  where chan = fromJust $ mChan m
        msg = mMsg m
        nik = fromJust $ mNick m
        nick = U.toString nik

getTitle :: String -> IO String
getTitle url = do
	putStrLn $ concat ["Getting ", url] 
	download url urlFile
	(_, Just kind, _, _) <- createProcess (proc "file" ["-b", urlFile]){ std_out = CreatePipe }
	kindoffile <- hGetContents kind
	let ftype = takeWhile (/=' ') kindoffile
	putStrLn $ concat ["It's a ", ftype, " document"]
	if ftype == "HTML" || ftype == "xHTML" then do
	html <- I.readFile urlFile
	let title = killSpaces $ flip stringRegex "(?<=>)[^<]*" $ stringRegex html "<[^>]*[tT][iI][tT][lL][eE][^>]*>[^<]*<[^>]*/[^>]*[tT][iI][tT][lL][eE][^>]*>"
	if length title > 0 then return title else return ""
	else return ""

tell :: MIrc -> B.ByteString -> String -> IO()
tell s chan nik = do
	isTellFile <- doesFileExist tellFile
	if isTellFile then do
	all <- I.readFile tellFile
	let messages = map (\a -> read a :: (String, String, String, String)) $ lines all
	let tells = map (\(a,b,c,t) -> concat $ [t, " UTC <",b,"> tell ",nik," ",c]) $ filter (\(a,b,c,t) -> isPrefixOf (lower a) (lower nik)) messages
	goTell s chan tells
	I.writeFile tellFile $ unlines $ map (show) $ filter (\(a,b,c,t) -> not (isPrefixOf (lower a) (lower nik))) messages
	else I.writeFile tellFile ""
	where
		goTell _ _ [] = return ()
		goTell s chan (t:ts) = do 
			sendMsg s chan $ U.fromString t
			goTell s chan ts

killSpaces :: String -> String
killSpaces [] = ""
killSpaces (a:[]) = if a=='\t' || a=='\n' then [] else [a]
killSpaces (a:b:ss) = if (a == ' ' && b == ' ') || a=='\t' || a=='\n' then killSpaces $ b:ss else a: (killSpaces $ b:ss)

runCmd :: String -> [String] -> IO ()
runCmd cmd options = do
	(_, Just force, _, _) <- createProcess (proc cmd options){ std_out = CreatePipe }
	runForce <- hGetContents force
	putStrLn runForce

download :: String -> String -> IO()
download url file = runCmd "curl" ["-sSL", "--user-agent","Mozilla/4.0", "-o",file, url]

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
dieMulti a = if length b == 0 then 1 else read b ::Int
	where b = stringRegex a "([0-9]+)?(?=d)"

dieOffset :: String -> Int
dieOffset a = if length b == 0 then 0 else if head b == '+' then read $ drop 1 b ::Int else read b ::Int
	where b = stringRegex a "(\\+|-)[0-9]+"

dieLoop :: String -> Int
dieLoop a = if length b == 0 then 1 else read b ::Int
	where b = stringRegex a "[0-9]+(?=\\|)"

dieD :: String -> Int
dieD a = if b == "%" then 100 else read b ::Int
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

boolRegex :: String -> String -> Bool
boolRegex orig regex = orig =~ regex :: Bool

spaceToPlus :: String -> String
spaceToPlus = map (\a -> if a==' ' then '+' else a)

address :: String -> String -> B.ByteString
address nik s = U.fromString $ concat [nik, ": ", s]

secondBuffer :: String -> String
secondBuffer [] = "00"
secondBuffer (s:[]) = '0':s:""
secondBuffer s = s

time :: Int -> String
time i = concat [show minutes, ":", secondBuffer $ show seconds]
	where
		minutes = floor $ flip (/) 60 $ fromIntegral i
		seconds = i - (minutes*60)

lower :: String -> String
lower = map (toLower)

events = [(Privmsg onMessage)]

main = do
  connect freenode False True

