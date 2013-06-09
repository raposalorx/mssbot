{-# LANGUAGE OverloadedStrings #-}
module Events
( events
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
  | B.isPrefixOf "?g " msg = (getRedirectTitle . googlestr . spaceToPlus . killSpaces . stringDropCmd $ msg) >>= mention
  | B.isPrefixOf "?wik " msg = (getRedirectTitle . wikistr . spaceToPlus . killSpaces . stringDropCmd $ msg) >>= mention
  | B.isPrefixOf "?tube " msg = (getRedirectTitle . youstr . spaceToPlus . killSpaces . stringDropCmd $ msg) >>= mention
  | B.isPrefixOf "?tell " msg = saveTell msg nick >>= mention
  | B.isPrefixOf "?h" msg = say . maybe helpstr id . flip lookup helpstrs . takeWhile (/=' ') . stringDropCmd $ msg
  | B.isPrefixOf "?ping" msg = ping msg >>= mention
  | B.isPrefixOf "?d " msg = (dice . stringDropCmd $ msg) >>= mention
  | B.isPrefixOf "?t" msg = title msg chan >>= mention
  | otherwise = do
    tell s nick
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
        mention = say . address nick

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

dice :: String -> IO String
dice msg = collapseroll $ map droll $ wrapDie $ matchDice msg
  where droll :: (Int, Int, Int, Int) -> IO [Int]
        droll (_, _, _, 0) = return []
        droll (d, multi, offset, num) = do
          m <- (dmulti d multi)
          m2 <- droll (d, multi, offset, num-1)
          return $ (m+offset):m2
        dmulti _ 0 = return 0
        dmulti dm mul = do
          r <- roll dm
          r2 <- dmulti dm (mul-1)
          return $ r+r2
        collapseroll :: [IO [Int]] -> IO String
        collapseroll [] = return ""
        collapseroll (i:is) = do
          a <- i
          b <- collapseroll is
          return $ unwords [(unwords $ map (show) a),b]
