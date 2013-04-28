{-# LANGUAGE OverloadedStrings #-}
module Events
( events
, onRaw
, onMessage
) where

import IO
import Text

import Network.SimpleIRC
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Control.Applicative

events = [(Privmsg onMessage){-,(Disconnect onDisconnect)-},(RawMsg onRaw)]

onRaw s m = putStrLn $ show m

onMessage :: EventFunc
onMessage s m
  | msg == "?h" = say helpstr
  | B.isPrefixOf "?h " msg = help s m $ takeWhile (/=' ') $ stringDropCmd msg
  | B.isPrefixOf "?ping" msg = say . (address nick) =<< ping msg
  | otherwise = return ()
  where channel = fromJust $ mChan m
        chan = U.toString channel
        msg = mMsg m
        nik = fromJust $ mNick m
        nick = U.toString nik
        say = send s m

ping :: B.ByteString -> IO String
ping msg = do
  let url = matchUrl . stringDropCmd $ msg
  if length url > 0 then do
    time <- flip stringRegex "(?<=time=)[0-9]*" <$> runCmd "ping" ["-c 2 -w 3"] url
    return $ if length time > 0 then concat [time, "ms"] else "Can't connect."
  else return "pong!"
