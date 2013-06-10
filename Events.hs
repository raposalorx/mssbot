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

events = [Privmsg onMessage,Disconnect onDisconnect,RawMsg onRaw]

onRaw s = print

onDisconnect mIrc = do
    m <- reconnect mIrc
    either (\err -> putStrLn $ "Unable to reconnect: " ++ show err) (\_ -> putStrLn "Successfully reconnected.") m

onMessage :: EventFunc
onMessage s m
  | iscmd "?g " = (getRedirectTitle . googlestr . spaceToPlus . killSpaces . stringDropCmd $ msg) >>= mention
  | iscmd "?wik " = (getRedirectTitle . wikistr . spaceToPlus . killSpaces . stringDropCmd $ msg) >>= mention
  | iscmd "?tube " = (getRedirectTitle . youstr . spaceToPlus . killSpaces . stringDropCmd $ msg) >>= mention
  | iscmd "?tell " = saveTell msg nick >>= mention
  | iscmd "?h" = say . fromMaybe helpstr . flip lookup helpstrs . takeWhile (/=' ') . stringDropCmd $ msg
  | iscmd "?ping" = ping msg >>= mention
  | iscmd "?d " = (dice . stringDropCmd $ msg) >>= mention
  | iscmd "?t" = title msg chan >>= mention
  | otherwise = do
    tell s nick
    let url = matchUrl . B.unpack $ msg
    unless (null url) $ do
      title <- getTitle url
      unless (null title) $ do
        tfile <- getTitleFile chan
        I.writeFile tfile title
  where iscmd = flip B.isPrefixOf msg
        channel = fromJust $ mChan m
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
  unescapeEntities <$> if not . null $ url then getTitle url else
    return $ either (const "No title to get") id e

ping :: B.ByteString -> IO String
ping msg = do
  let url = matchUrl . stringDropCmd $ msg
  if not . null $ url then do
    time <- flip stringRegex "(?<=time=)[0-9]*" <$> runCmd "ping" ["-c 2 -w 3", url] ""
    return $ if not . null $ time then time ++ "ms" else "Can't connect."
  else return "pong!"

dice :: String -> IO String
dice msg = fmap collapseroll $ mapM droll $ wrapDie $ matchDice msg
  where droll :: (Int, Int, Int, Int) -> IO [Int]
        droll (d, multi, offset, num) = fmap (map (+offset)) $ replicateM num (
                                          fmap sum $ replicateM multi (roll d))
        collapseroll :: [[Int]] -> String
        collapseroll [] =  ""
        collapseroll (i:is) = unwords [unwords $ map show i, collapseroll is]
