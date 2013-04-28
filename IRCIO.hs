module IRCIO
( helpstr
, help
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
          "tell <nick> <message>, " ++
          "ping [url], " ++
          "dc <equation>, " ++
          "eval <expression>, " ++
          "t [url], " ++
--          "trans <string>, " ++
          "g <query>, " ++
          "wik <query>, " ++
          "tube <query>, " ++
          "weather <location>[,province[,country]], " ++
          "d <[x|]<y>d<z>[+/-w]>..., tatl #; " ++
          "Passive: Report titles for urls;"

help s m "h"		= send s m "?h [command] - A help dialog for command, Or a list of commands."
help s m "tell"		= send s m "?tell <nick> <message> - Send \"<nick> tell message\" as a PM to nick next time they speak."
help s m "ping"		= send s m "?ping [url] - Ping a site and return it's response time. Or just pong the user."
help s m "dc"       = send s m "?dc <equation> - Arbitrary precision reverse polish calculator."
help s m "eval"     = send s m "?eval <expression> - Haskell expression"
help s m "t"		= send s m "?t [url] - Gets either url or the previous URL from the channel."
--help s m "trans"    = send s m "?trans <string> - Translate string into english."
help s m "g"		= send s m "?g <query> - Return the first google search result matching query."
help s m "wik"		= send s m "?wik <query> - Return the first wikipedia search result matching query."
help s m "tube"		= send s m "?tube <query> - Return the first youtube search result matching query."
help s m "weather"	= send s m "?weather <location>[,province[,country]] - Get the weather from location."
help s m "d"		= send s m "?d <[x|]<y>d<z>[+/-w]>... - Sum of the values of y dice with z sides, plus or minus w, x times."
--help s m "tatl"     = send s m "?tatl # - Link the sentance numbered # from tatoeba.org"
help s m _          = send s m helpstr

send :: MIrc -> IrcMessage -> String -> IO()
send s m msg = do
    let chan = fromJust $ mChan m
    nick <- getNickname s
    let from = fromJust $ mNick m
    let to = U.toString $ if nick == chan then from else chan
    sendRaw s $ U.fromString $ concat ["PRIVMSG ", to, " :", msg]

runCmd :: String -> [String] -> String -> IO String
runCmd cmd args stdin = do
    (exit,out,err) <- readProcessWithExitCode cmd args stdin
    putStrLn $ show exit
    if exit==ExitSuccess then return out else return "Command Failed"
