module Course.Hw02.LogAnalysis (parseMessage, parse, whatWentWrong) where

import Course.Hw02.Log (LogMessage (..), MessageType (..), MessageTree (Leaf, Node))

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  "I":ts:xs -> LogMessage Info (read ts) (unwords xs)
  "W":ts:xs -> LogMessage Warning (read ts) (unwords xs)
  "E":e:ts:xs -> LogMessage (Error (read e)) (read ts) (unwords xs)
  _ -> Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

timestamp :: LogMessage -> Int
timestamp lm = case lm of
  LogMessage Info ts _ -> ts
  LogMessage Warning ts _ -> ts
  LogMessage (Error _) ts _ -> ts
  Unknown _ -> 0

insert :: LogMessage -> MessageTree -> MessageTree
insert lm t = case lm of
  Unknown _ -> t
  _ -> case t of
    Leaf -> Node Leaf lm Leaf
    Node l lm' r -> if timestamp lm < timestamp lm'
      then Node (insert lm l) lm' r
      else Node l lm' (insert lm r)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder t = case t of
  Leaf -> []
  Node l lm r -> inOrder l ++ [lm] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lst = map message (inOrder (build (filter severity50 lst)))
  where 
    severity50 lm = case lm of
      LogMessage (Error e) _ _ -> e >= 50
      _ -> False

message :: LogMessage -> String
message m = case m of
  LogMessage Info _ s -> s
  LogMessage Warning _ s -> s
  LogMessage (Error _) _ s -> s
  Unknown s -> s
