module Course.Hw02.LogAnalysis (parseMessage, parse) where

import Course.Hw02.Log (LogMessage (..), MessageType (..))

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  "I":ts:xs -> LogMessage Info (read ts) (unwords xs)
  "W":ts:xs -> LogMessage Warning (read ts) (unwords xs)
  "E":e:ts:xs -> LogMessage (Error (read e)) (read ts) (unwords xs)
  _ -> Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

