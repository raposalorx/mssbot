module IO
( helpstr
, help
, address
, send
, runCmd
) where

import Network.SimpleIRC
import Data.Maybe
import System.Process
import System.Exit
import qualified Data.ByteString.UTF8 as U

helpstr = "Commands (prefix ?): " ++
          "h [command] (help), " ++
--          "tell <nick> <message>, " ++
          "ping [url], " ++
--          "dc <equation>, " ++
--          "eval <expression>, " ++
--          "t [url], " ++
--          "trans <string>, " ++
--          "g <query>, " ++
--          "wik <query>, " ++
--          "tube <query>, " ++
--          "weather <location>[,province[,country]], " ++
--          "d <[x|]<y>d<z>[+/-w]>..., tatl #; " ++
--          "Passive: Report titles for urls;"
          ""

help s m t  | t == "h" = say "?h [command] - A help dialog for command, Or a list of commands."
--help s m t  | t == "tell" = say "?tell <nick> <message> - Send \"<nick> tell message\" as a PM to nick next time they speak."
help s m t  | t == "ping" = say "?ping [url] - Ping a site and return it's response time. Or just pong the user."
--help s m t  | t == "dc"       = say "?dc <equation> - Arbitrary precision reverse polish calculator."
--help s m t  | t == "eval"     = say "?eval <expression> - Haskell expression"
--help s m t  | t == "t"		= say "?t [url] - Gets either url or the previous URL from the channel."
--help s m t  | t == "trans"    = say "?trans <string> - Translate string into english."
--help s m t  | t == "g"		= say "?g <query> - Return the first google search result matching query."
--help s m t  | t == "wik"		= say "?wik <query> - Return the first wikipedia search result matching query."
--help s m t  | t == "tube"		= say "?tube <query> - Return the first youtube search result matching query."
--help s m t  | t == "weather"	= say "?weather <location>[,province[,country]] - Get the weather from location."
--help s m t  | t == "d"		= say "?d <[x|]<y>d<z>[+/-w]>... - Sum of the values of y dice with z sides, plus or minus w, x times."
--help s m t  | t == "tatl"     = say "?tatl # - Link the sentance numbered # from tatoeba.org"
            | otherwise =  say helpstr
  where say = send s m

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
runCmd cmd args var= do
    (exit,out,err) <- readProcessWithExitCode cmd (args++[var]) ""
    putStrLn $ show exit
    if exit==ExitSuccess then return out else return "Command Failed"
