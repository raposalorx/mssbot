{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Data.Maybe
import Data.String
import Data.List
import GHC.Exts( IsString(..) )
import Text.Regex.PCRE
import Network.HTTP.Wget
import System.IO
import System.Exit
import System.Process
import System.Random
import Language.Translate.Google
import Web.Encodings
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as U

killSpaces :: String -> String
killSpaces [] = ""
killSpaces (a:[]) = if a=='\t' then [] else [a]
killSpaces (a:b:ss) = if (a == ' ' && b == ' ') || a=='\t' then killSpaces $ b:ss else a: (killSpaces $ b:ss)

runCmd :: String -> [String] -> IO ()
runCmd cmd options = do
	(_, Just force, _, _) <- createProcess (proc cmd options){ std_out = CreatePipe }
	runForce <- hGetContents force
	putStrLn runForce

getTitle :: String -> IO String
getTitle url = do 
	runCmd "curl" ["-sSL", "--user-agent","Mozilla/4.0", "-o","/var/tmp/mssboturltmp", url]
	(_, Just kind, _, _) <- createProcess (proc "file" ["-b", "/var/tmp/mssboturltmp"]){ std_out = CreatePipe }
	kindoffile <- hGetContents kind
	let ftype = takeWhile (/=' ') kindoffile
	if ftype == "HTML" || ftype == "xHTML" then do
	hin <- openFile "/var/tmp/mssboturltmp" ReadMode
	tloop hin
	else return ""
	where
		tloop h = do
			html <- hGetContents h
			let title0 = html =~ ("<[^>]*[tT][iI][tT][lL][eE][^>]*>[^<]*<[^>]*/[^>]*[tT][iI][tT][lL][eE][^>]*>"::String) :: String
			let title1 = title0 =~ ("(?<=>)[^<]*"::String) :: String
			let title = killSpaces title1 
			if length title > 0 then return title else return ""

tell :: MIrc -> B.ByteString -> B.ByteString -> IO()
tell s chan nik = do
	hin <- openFile "/var/tmp/mssbottelllist" ReadMode
	all <- hGetContents hin
	let messages = map (\a -> read a :: (String, String, String)) $ lines all
	let tells = map (\(a,b,c) -> concat $ ["<",b,"> tell ",U.toString nik," ",c]) $ filter (\(a,b,c) -> isPrefixOf a (U.toString nik)) messages
	goTell s chan tells
	hout <- openFile "/var/tmp/mssbottelllist" WriteMode
	hPutStr hout $ unlines $ map (\a -> show a) $ filter (\(a,b,c) -> not (isPrefixOf a (U.toString nik))) messages
	hClose hin
	hClose hout
	where
		goTell _ _ [] = return ()
		goTell s chan (t:ts) = do 
			sendMsg s chan $ U.fromString t
			goTell s chan ts

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
	return $ unwords [(unwords $ map (\c -> show c) a),b]

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

onMessage :: EventFunc
onMessage s m
  | msg == "?h" = do
    sendMsg s chan "Commands (prefix ?): h (help), tell <nick> <message>, ping, t <string> (translate), g <query> (google), wik <query>, weather <location>[,province], d <[x|]<y>d<z>[+/-w]>... (dice); Passive: Report titles for urls;"
  | msg == "?ping" = sendMsg s chan $ U.fromString $ unwords [U.toString nik,": pong!"]
  | B.isPrefixOf "?tell " msg = do
		let (nick, message) = span (/=' ') $ stringDropCmd msg
		hout <- openFile "/var/tmp/mssbottelllist" AppendMode
		hPutStr hout $ unwords [show (nick, U.toString nik, concat [dropWhile (==' ') message]),"\n"]
		hClose hout
		sendMsg s chan $ U.fromString $ unwords ["I'll totally pass that on for you!"]
  | B.isPrefixOf "?t " msg = do
		trans <- translate (dropCommand msg) Nothing English
		sendMsg s chan $ U.fromString $ unwords [U.toString nik,":", decodeHtml $ U.toString $ either (\e -> U.fromString e) (\r -> r) trans]
  | B.isPrefixOf "?d " msg = do
		ds <- collapseroll $ map (droll) $  map (\a -> (let b = stringRegex a "(?<=d)([0-9]+|%)" in if b == "%" then 100 else read b ::Int, let b = stringRegex a "([0-9]+)?(?=d)" in if length b == 0 then 1 else read b ::Int, let b = stringRegex a "(\\+|-)[0-9]+" in if length b == 0 then 0 else if head b == '+' then read $ drop 1 b ::Int else read b ::Int, let b = stringRegex a "[0-9]+(?=\\|)" in if length b == 0 then 1 else read b ::Int)) $ map (\a -> head a) $ listRegex (stringDropCmd msg) "([0-9]?\\|)?([0-9]+)?d([0-9]+|%)((\\+|-)[0-9]+)?"
		sendMsg s chan $ U.fromString ds
  | B.isPrefixOf "?g " msg = do
		let search = concat ["http://ajax.googleapis.com/ajax/services/search/web?v=1.0&safe=off&q=", map (\a -> if a==' ' then '+' else a) $ stringDropCmd msg]
		runCmd "curl" ["-sS", "--user-agent","Mozilla/4.0", "-o","/var/tmp/mssbotgoogletmp", search]
		hin <- openFile "/var/tmp/mssbotgoogletmp" ReadMode
		redir <- hGetContents hin
		sendMsg s chan $ U.fromString $ unwords [U.toString nik, ":", stringRegex redir "(?<=\"url\":\")[^\"]*"]
		hClose hin
  | B.isPrefixOf "?wik " msg = do
		let search = concat ["http://ajax.googleapis.com/ajax/services/search/web?v=1.0&safe=off&q=%3Asite+www.wikipedia.com+", map (\a -> if a==' ' then '+' else a) $ stringDropCmd msg]
		runCmd "curl" ["-sS", "--user-agent","Mozilla/4.0", "-o","/var/tmp/mssbotgoogletmp", search]
		hin <- openFile "/var/tmp/mssbotgoogletmp" ReadMode
		redir <- hGetContents hin
		sendMsg s chan $ U.fromString $ unwords [U.toString nik, ":", stringRegex redir "(?<=\"url\":\")[^\"]*"]
		hClose hin
  | B.isPrefixOf "?weather " msg = do
		let search = concat ["http://www.google.com/ig/api?weather=", map (\a -> if a==' ' then '+' else a) $ stringDropCmd msg]
		runCmd "curl" ["-sS", "--user-agent","Mozilla/4.0", "-o","/var/tmp/mssbotgoogletmp", search]
		hin <- openFile "/var/tmp/mssbotgoogletmp" ReadMode
		redir <- hGetContents hin
		if boolRegex redir "city data" then do
		let his = map (\a -> read a ::Int) $ map (head) $ listRegex redir "(?<=<high data=\")[^\"]*"
		let lows = map (\a -> read a ::Int) $ map (head) $ listRegex redir "(?<=<low data=\")[^\"]*"
		let conditions = map (head) $ listRegex redir "(?<=<condition data=\")[^\"]*"
		let form = concat [stringRegex redir "(?<=<city data=\")[^\"]*", ": ", stringRegex redir "(?<=<temp_c data=\")[^\"]*", "C ", show $ fToC $ his!!0, "H ", show $ fToC $ lows!!0, "L ", stringRegex redir "(?<=<wind_condition data=\")[^\"]*"," and ", conditions!!0, ", expect ", conditions!!1]
		sendMsg s chan $ U.fromString $ unwords [U.toString nik, ":", form]
		else putStrLn search
  | otherwise = do
		let message = B.unpack msg
		let url = stringRegex message "(http(s)?://)(www.)?([a-zA-Z0-9\\-_]{1,}\\.){1,}(com|ca|uk|fr|net|org|ru|me|gov|co|jp|info)(/)?[^ ]*"
		tell s chan nik
		if length url > 0 then do
		title <- getTitle url
		sendMsg s chan $ U.fromString title
		else putStrLn $ show m
  where chan = fromJust $ mChan m
        msg = mMsg m
        nik = fromJust $ mNick m
        
events = [(Privmsg onMessage)]

freenode = defaultConfig
  { cAddr = "irc.freenode.net"
  , cNick = "mssbotdev"
  , cChannels = ["##mssdev", "#maelstrom"]
  , cEvents = events
  }

main = do
  connect freenode False True
