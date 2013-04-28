module Text
( stringDropCmd
, matchUrl
, stringRegex
) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as U
import Text.Regex.PCRE

regexUrl = "(http(s)?://)?(www.)?([a-zA-Z0-9\\-_]{1,}\\.){1,}[a-zA-Z]{2,4}(/)?[^ ]*"

matchUrl :: String -> String
matchUrl = flip stringRegex regexUrl

stringRegex :: String -> String -> String
stringRegex orig regex = orig =~ regex

listRegex :: String -> String -> [[String]]
listRegex orig regex = orig =~ regex :: [[String]]

dropCommand :: B.ByteString -> B.ByteString
dropCommand b = B.drop 1 $ B.dropWhile (/= ' ') b

stringDropCmd :: B.ByteString -> String
stringDropCmd = U.toString . dropCommand
