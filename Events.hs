{-# LANGUAGE OverloadedStrings #-}
module Events
( events
, onRaw
, onMessage
) where

import IRCIO
import Text

import Network.SimpleIRC
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Char8 as B
import Data.Maybe

events = [(Privmsg onMessage){-,(Disconnect onDisconnect)-},(RawMsg onRaw)]

onRaw s m = putStrLn $ show m

onMessage :: EventFunc
onMessage s m
  | msg == "?h" = send s m helpstr
  | B.isPrefixOf "?h " msg = help s m $ takeWhile (/=' ') $ stringDropCmd msg
  | otherwise = return ()
  where channel = fromJust $ mChan m
        chan = U.toString channel
        msg = mMsg m
        nik = fromJust $ mNick m
        nick = U.toString nik
