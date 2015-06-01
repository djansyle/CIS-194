module LogAnalysis where

import Log
import Data.List(sortBy)

parseMessage :: String -> LogMessage
parseMessage message =
    case words message of
        ("I":time:msg) -> LogMessage (Info) (read time :: Int) (unwords msg)
        ("W":time:msg) -> LogMessage (Warning) (read time :: Int) (unwords msg)
        ("E":code:time:msg) -> LogMessage (Error (read code :: Int)) (read time :: Int)  (unwords msg)
        msg -> Unknown (unwords msg)

parse :: String -> [LogMessage]
parse = map parseMessage . lines


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) x = x
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ mt _) (Node left rmsg@(LogMessage _ rt _) right)
    | mt > rt = Node left rmsg (insert msg right)
    | otherwise = Node (insert msg left) rmsg right

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left log right) = inOrder left ++ [log] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs =  message $ sortList $ severeError logs
      where
         sortList :: [LogMessage] -> [LogMessage]
         sortList unsorted = sortBy (\(LogMessage (Error _) ftime _) (LogMessage (Error _) stime _) -> compare ftime stime) unsorted

         severeError :: [LogMessage] -> [LogMessage]
         severeError errs =  filter (\(LogMessage (Error level) _ _) -> level >= 50 ) errs

         message :: [LogMessage] -> [String]
         message logs = map (\(LogMessage _ _ msg) -> msg) logs


