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
import qualified System.IO.UTF8 as I
import System.Exit
import System.Process
import System.Random
import System.Directory
import Language.Translate.Google
import Web.Encodings
import Network.MPD
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as U

killSpaces :: String -> String
killSpaces [] = ""
killSpaces (a:[]) = if a=='\t' || a=='\n' then [] else [a]
killSpaces (a:b:ss) = if (a == ' ' && b == ' ') || a=='\t' || a=='\n' then killSpaces $ b:ss else a: (killSpaces $ b:ss)

runCmd :: String -> [String] -> IO ()
runCmd cmd options = do
	(_, Just force, _, _) <- createProcess (proc cmd options){ std_out = CreatePipe }
	runForce <- hGetContents force
	putStrLn runForce

getTitle :: String -> IO String
getTitle url = do
	putStrLn $ concat ["Getting ", url] 
	runCmd "curl" ["-sSL", "--user-agent","Mozilla/4.0", "-o","/var/tmp/mssboturltmp", url]
	(_, Just kind, _, _) <- createProcess (proc "file" ["-b", "/var/tmp/mssboturltmp"]){ std_out = CreatePipe }
	kindoffile <- hGetContents kind
	let ftype = takeWhile (/=' ') kindoffile
	putStrLn $ concat ["It's a ", ftype, " document"]
	if ftype == "HTML" || ftype == "xHTML" then do
--	hin <- openFile "/var/tmp/mssboturltmp" ReadMode
--	hSetEncoding hin utf8
	hin <- I.readFile "/var/tmp/mssboturltmp"
	tloop hin
	else return ""
	where
		tloop html = do
--			html <- hGetContents h
			let title0 = stringRegex html "<[^>]*[tT][iI][tT][lL][eE][^>]*>[^<]*<[^>]*/[^>]*[tT][iI][tT][lL][eE][^>]*>"
			putStrLn title0
			let title1 = stringRegex title0 "(?<=>)[^<]*"
			putStrLn title1
			let title = killSpaces title1 
			if length title > 0 then return title else return ""

tell :: MIrc -> B.ByteString -> String -> IO()
tell s chan nik = do
	isTellFile <- doesFileExist "/var/tmp/mssbottelllist"
	if isTellFile then do
--	hin <- openFile "/var/tmp/mssbottelllist" ReadMode
--	all <- hGetContents hin
	all <- I.readFile "/var/tmp/mssbottelllist"
	let messages = map (\a -> read a :: (String, String, String, String)) $ lines all
	let tells = map (\(a,b,c,t) -> concat $ [t, " UTC <",b,"> tell ",nik," ",c]) $ filter (\(a,b,c,t) -> isPrefixOf (lower a) (lower nik)) messages
	goTell s chan tells
--	hout <- openFile "/var/tmp/mssbottelllist" WriteMode
--	hPutStr hout $ unlines $ map (show) $ filter (\(a,b,c,t) -> not (isPrefixOf (lower a) (lower nik))) messages
	I.writeFile "/var/tmp/mssbottelllist" $ unlines $ map (show) $ filter (\(a,b,c,t) -> not (isPrefixOf (lower a) (lower nik))) messages
--	hClose hin
--	hClose hout
	else do
--	hout <- openFile "/var/tmp/mssbottelllist" WriteMode
--	hPutStr hout ""
--	hClose hout
	I.writeFile "/var/tmp/mssbottelllist" ""
	where
		goTell _ _ [] = return ()
		goTell s chan (t:ts) = do 
			sendMsg s chan $ U.fromString t
			goTell s chan ts

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
		if state == "Stopped" then sendMsg s chan $ U.fromString state else do
		sendMsg s chan $ U.fromString $ concat [state, ": ", song, " - ", artist, " [", curtime, "/", lentime, "]"]
  | B.isPrefixOf "?tell " msg = do
		let (mnick, message) = span (/=' ') $ stringDropCmd msg
		ftime <- getCurrentTime
		let time = stringRegex (show ftime) "[^\\.]*(?=:[0-9]{2}\\.)"
--		hout <- openFile "/var/tmp/mssbottelllist" AppendMode
--		hPutStr hout $ concat [show (mnick, nick, concat [dropWhile (==' ') message], time),"\n"]
--		hClose hout
		I.appendFile "/var/tmp/mssbotgoogletmp" $ concat [show (mnick, nick, concat [dropWhile (==' ') message], time),"\n"]
		sendMsg s chan $ U.fromString "I'll totally pass that on for you!"
  | B.isPrefixOf "?t " msg = do
		trans <- translate (dropCommand msg) Nothing English
		sendMsg s chan $ address nick $ decodeHtml $ U.toString $ either (\e -> U.fromString e) (\r -> r) trans
  | B.isPrefixOf "?d " msg = do
		ds <- collapseroll $ map (droll) $ map (\a -> (dieD a, dieMulti a, dieOffset a, dieLoop a)) $ map (head) $ listRegex (stringDropCmd msg) "([0-9]?\\|)?([0-9]+)?d([0-9]+|%)((\\+|-)[0-9]+)?"
		sendMsg s chan $ address nick ds
  | B.isPrefixOf "?g " msg = do
		let search = concat ["http://ajax.googleapis.com/ajax/services/search/web?v=1.0&safe=off&q=", spaceToPlus $ stringDropCmd msg]
		runCmd "curl" ["-sS", "--user-agent","Mozilla/4.0", "-o","/var/tmp/mssbotgoogletmp", search]
--		hin <- openFile "/var/tmp/mssbotgoogletmp" ReadMode
--		redir <- hGetContents hin
		redir <- I.readFile "/var/tmp/mssbotgoogletmp"
		sendMsg s chan $ address nick $ stringRegex redir "(?<=\"url\":\")[^\"]*"
--		hClose hin
  | B.isPrefixOf "?wik " msg = do
		let search = concat ["http://ajax.googleapis.com/ajax/services/search/web?v=1.0&safe=off&q=%3Asite+www.wikipedia.com+", spaceToPlus $ stringDropCmd msg]
		runCmd "curl" ["-sS", "--user-agent","Mozilla/4.0", "-o","/var/tmp/mssbotgoogletmp", search]
--		hin <- openFile "/var/tmp/mssbotgoogletmp" ReadMode
--		redir <- hGetContents hin
		redir <- I.readFile "/var/tmp/mssbotgoogletmp"
		sendMsg s chan $ address nick $ stringRegex redir "(?<=\"url\":\")[^\"]*"
--		hClose hin
  | B.isPrefixOf "?weather " msg = do
		let search = concat ["http://www.google.com/ig/api?weather=", spaceToPlus $ stringDropCmd msg]
		runCmd "curl" ["-sS", "--user-agent","Mozilla/4.0", "-o","/var/tmp/mssbotgoogletmp", search]
--		hin <- openFile "/var/tmp/mssbotgoogletmp" ReadMode
--		redir <- hGetContents hin
		redir <- I.readFile "/var/tmp/mssbotgoogletmp"
		if boolRegex redir "city data" then do
		let his = map (\a -> read a ::Int) $ map (head) $ listRegex redir "(?<=<high data=\")[^\"]*"
		let lows = map (\a -> read a ::Int) $ map (head) $ listRegex redir "(?<=<low data=\")[^\"]*"
		let conditions = map (head) $ listRegex redir "(?<=<condition data=\")[^\"]*"
		let form = concat [stringRegex redir "(?<=<city data=\")[^\"]*", ": ", stringRegex redir "(?<=<temp_c data=\")[^\"]*", "C ", show $ fToC $ his!!0, "H ", show $ fToC $ lows!!0, "L ", stringRegex redir "(?<=<wind_condition data=\")[^\"]*"," and ", conditions!!0, ", expect ", conditions!!1]
		sendMsg s chan $ address nick form
		else putStrLn search
  | otherwise = do
		let message = B.unpack msg
		let url = stringRegex message "(http(s)?://)(www.)?([a-zA-Z0-9\\-_]{1,}\\.){1,}[a-zA-Z]{2,4}(/)?[^ ]*"-- "(com|ca|uk|fr|net|org|ru|me|gov|co|jp|info|cc)(/)?[^ ]*"
		tell s chan nick
		if length url > 0 then do
		title <- getTitle url
		sendMsg s chan $ U.fromString $ decodeHtml title
		else putStrLn $ show m
  where chan = fromJust $ mChan m
        msg = mMsg m
        nik = fromJust $ mNick m
        nick = U.toString nik
        
events = [(Privmsg onMessage)]

freenode = defaultConfig
  { cAddr = "irc.freenode.net"
  , cNick = "mssbotdev"
  , cChannels = ["##mssdev", "#maelstrom"]
  , cEvents = events
  }

main = do
  connect freenode False True