{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module LogAnalysis where

import Log
import Data.Char (isSpace)

parseRest :: String -> (Int, String)
parseRest string =
  let [(t, rest)] = lex string in
   (read t :: Int, dropWhile isSpace rest)

parseMessage :: String -> LogMessage
parseMessage message =
  let [(severity, rest)] = lex message
  in
   case severity of
    "I" -> let (ts, msg) = parseRest rest in LogMessage Info ts msg
    "W" -> let (ts, msg) = parseRest rest in LogMessage Warning ts msg
    "E" -> let (e, rest') = parseRest rest
           in
            let (ts, msg) = parseRest rest' in LogMessage (Error e) ts msg
    _   -> Unknown message

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines
