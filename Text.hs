module Text
( stringDropCmd
, matchUrl
, matchTitle
, stringRegex
, helpstr
, helpstrs
, unescapeEntities
, googlestr
, wikistr
, youstr
, spaceToPlus
, killSpaces
, lower
) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as U
import Text.Regex.PCRE
import Text.HTML.TagSoup.Entity (lookupEntity)
import Data.Char

helpstr = "Commands (prefix ?): " ++
          "h[elp] [command], " ++
--          "tell <nick> <message>, " ++
          "ping [url], " ++
--          "dc <equation>, " ++
--          "eval <expression>, " ++
          "t[itle] [url], " ++
--          "trans <string>, " ++
          "g <query>, " ++
          "wik <query>, " ++
          "tube <query>, " ++
--          "weather <location>[,province[,country]], " ++
--          "d <[x|]<y>d<z>[+/-w]>..., tatl #; " ++
--          "Passive: Report titles for urls;"
          ""

helpstrs =  [("h", "?h[elp] [command] - A help dialog for command, Or a list of commands.")
            ,("ping", "?ping [url] - Ping a site and return it's response time. Or just pong the user.")
--            ,("tell", "?tell <nick> <message> - Send \"<nick> tell message\" as a PM to nick next time they speak.")
--            ,("dc", "?dc <equation> - Arbitrary precision reverse polish calculator.")
--            ,("eval", "?eval <expression> - Haskell expression")
            ,("t", "?t[itle] [url] - Gets either url or the previous URL from the channel.")
--            ,("trans", "?trans <string> - Translate string into english.")
            ,("g", "?g <query> - Return the first google search result matching query.")
            ,("wik", "?wik <query> - Return the first wikipedia search result matching query.")
            ,("tube", "?tube <query> - Return the first youtube search result matching query.")
--            ,("weather", "?weather <location>[,province[,country]] - Get the weather from location.")
--            ,("d", "?d <[x|]<y>d<z>[+/-w]>... - Sum of the values of y dice with z sides, plus or minus w, x times.")
--            ,("tatl", "?tatl # - Link the sentance numbered # from tatoeba.org")
            ]

regexUrl = "(http(s)?://)?(www.)?([a-zA-Z0-9\\-_]{1,}\\.){1,}[a-zA-Z]{2,4}(/)?[^ ]*"

regexTitle = "<[^>]*[tT][iI][tT][lL][eE][^>]*>[^<]*<[^>]*/[^>]*[tT][iI][tT][lL][eE][^>]*>"

googlestr = (++) "http://ajax.googleapis.com/ajax/services/search/web?v=1.0&safe=off&q="

wikistr = (++) . googlestr $ "site%3Awikipedia.org+"

youstr = (++) . googlestr $ "site%3Awww.youtube.com+"

matchUrl :: String -> String
matchUrl = flip stringRegex regexUrl

matchTitle :: String -> String
matchTitle = killSpaces . map (\x -> if x=='\r' then ' ' else x) . flip stringRegex "(?<=>)[^<]*" . flip stringRegex regexTitle . unwords . lines

stringRegex :: String -> String -> String
stringRegex orig regex = orig =~ regex

listRegex :: String -> String -> [[String]]
listRegex orig regex = orig =~ regex :: [[String]]

dropCommand :: B.ByteString -> B.ByteString
dropCommand b = B.drop 1 $ B.dropWhile (/= ' ') b

stringDropCmd :: B.ByteString -> String
stringDropCmd = U.toString . dropCommand

killSpaces :: String -> String
killSpaces [] = ""
killSpaces (a:[]) = if a=='\t' || a=='\n' then [] else [a]
killSpaces (a:b:ss) = (if (a == ' ' && b == ' ') || a=='\t' || a=='\n' then (\z->z) else (a:)) $ killSpaces $ b:ss

unescapeEntities :: String -> String
unescapeEntities [] = []
unescapeEntities ('&':xs) = 
  let (b, a) = break (== ';') xs in
  case (lookupEntity b, a) of
    (Just c, ';':as) ->  c  : unescapeEntities as    
    _                -> '&' : unescapeEntities xs
unescapeEntities (x:xs) = x : unescapeEntities xs

spaceToPlus :: String -> String
spaceToPlus = map stp . killSpaces
  where stp ' ' = '+'
        stp x = x

lower = map toLower

