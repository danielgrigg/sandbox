{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- | this is a documentation string for parsemessage
parseMessage :: String -> LogMessage

parseMessage message = 
  case (words message) of
    "E":severity:timestamp:messageWords -> 
      LogMessage (Error (read severity)) (read timestamp) (unwords messageWords)
    "W":timestamp:messageWords -> 
      LogMessage Warning (read timestamp) (unwords messageWords)
    "I":timestamp:messageWords -> 
      LogMessage Info (read timestamp) (unwords messageWords)
    _ -> Unknown message 

parse :: String -> [LogMessage]
parse = parseLines . lines 

parseLines :: [String] -> [LogMessage]
parseLines [] = []
parseLines (line:rest) = (parseMessage line):(parseLines rest)

insert :: LogMessage -> MessageTree -> MessageTree

insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert message@(LogMessage _ t _) (Node left node@(LogMessage _ tNode _) right) =
  if (t < tNode) 
    then (Node (insert message left) node right)
    else (Node left node (insert message right))
insert message (Node _ (Unknown _) _) = Node Leaf message Leaf

{-ls = [LogMessage Info 30 "30", -}
      {-LogMessage Info 20 "20",-}
      {-Unknown "foo",-}
      {-LogMessage (Error 20) 99 "99",-}
      {-LogMessage (Error 70) 70 "70",-}
      {-LogMessage Info 10 "10",-}
      {-LogMessage Info 15 "15",-}
      {-LogMessage Info 50 "50",-}
      {-LogMessage Info 40 "40"]-}

build :: [LogMessage] -> MessageTree
build messages = buildTree messages Leaf

buildTree :: [LogMessage] -> MessageTree -> MessageTree
buildTree [] tree = tree
buildTree (message:rest) tree = buildTree rest (insert message tree)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = 
  inOrder left ++ [message] ++ inOrder right

orderLogs :: [LogMessage] -> [LogMessage]
orderLogs = inOrder . build
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map getMessage) . filterError50 . orderLogs

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ message) = message
getMessage (Unknown message) = message

filterError50 :: [LogMessage] -> [LogMessage]
filterError50 [] = []
filterError50 (m@(LogMessage (Error severity) _ _):rest)
  | severity >= 50 = m:(filterError50 rest)
filterError50 (_:rest) = filterError50 rest

