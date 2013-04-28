module Text
( stringDropCmd
) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as U

dropCommand :: B.ByteString -> B.ByteString
dropCommand b = B.drop 1 $ B.dropWhile (/= ' ') b

stringDropCmd :: B.ByteString -> String
stringDropCmd = U.toString . dropCommand
