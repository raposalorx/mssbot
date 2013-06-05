{-# LANGUAGE OverloadedStrings #-}
module Events
( events
, onRaw
, onMessage
, onDisconnect
) where

import IO
import Text

import Network.SimpleIRC
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Control.Applicative
import qualified System.IO.UTF8 as I
import Control.Exception
import Control.Monad
import System.IO.Error

events = [(Privmsg onMessage),(Disconnect onDisconnect),(RawMsg onRaw)]

onRaw s m = putStrLn $ show m

onDisconnect mIrc = do
    m <- reconnect mIrc
    either (\err -> putStrLn $ "Unable to reconnect: " ++ show err) (\_ -> putStrLn "Successfully reconnected.") m

onMessage :: EventFunc
onMessage s m
  | B.isPrefixOf "?g " msg = say . address nick =<< (getRedirectTitle . googlestr . spaceToPlus . killSpaces . stringDropCmd $ msg)
  | B.isPrefixOf "?wik " msg = say . address nick =<< (getRedirectTitle . wikistr . spaceToPlus . killSpaces . stringDropCmd $ msg)
  | B.isPrefixOf "?tube " msg = say . address nick =<< (getRedirectTitle . youstr . spaceToPlus . killSpaces . stringDropCmd $ msg)
  | B.isPrefixOf "?h" msg = say . maybe helpstr id . flip lookup helpstrs . takeWhile (/=' ') . stringDropCmd $ msg
  | B.isPrefixOf "?ping" msg = say . address nick =<< ping msg
  | B.isPrefixOf "?t" msg = say . address nick =<< title msg chan
  | otherwise = do
    let url = matchUrl . B.unpack $ msg
    if length url > 0 then do
      title <- getTitle url
      if length title > 0 then do
        tfile <- getTitleFile chan
        I.writeFile tfile title
      else return ()
    else return ()
  where channel = fromJust $ mChan m
        chan = U.toString channel
        msg = mMsg m
        nik = fromJust $ mNick m
        nick = U.toString nik
        say = send s m

title :: B.ByteString -> String -> IO String
title msg chan = do
  let url = matchUrl . stringDropCmd $ msg
  tfile <- getTitleFile chan
  e <- tryJust (guard . isDoesNotExistError) (I.readFile tfile)
  unescapeEntities <$> if length url > 0 then getTitle url else
    return $ either (const "No title to get") (id) e

ping :: B.ByteString -> IO String
ping msg = do
  let url = matchUrl . stringDropCmd $ msg
  if length url > 0 then do
    time <- flip stringRegex "(?<=time=)[0-9]*" <$> runCmd "ping" ["-c 2 -w 3", url] ""
    return $ if length time > 0 then concat [time, "ms"] else "Can't connect."
  else return "pong!"
