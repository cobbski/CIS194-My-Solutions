-- CIS 194 - Homework 2
module LogAnalysis where

import Log
import Text.Read
import Data.Maybe

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ timestamp _) (Node l val@(LogMessage _ nodetime _) r)
    | timestamp > nodetime = Node l val (insert msg r)
    | timestamp < nodetime = Node (insert msg l) val r

buildRecursive :: [LogMessage] -> MessageTree -> MessageTree
buildRecursive [] tree = tree
buildRecursive (logMsg : s) tree = buildRecursive s (insert logMsg tree)

build :: [LogMessage] -> MessageTree
build logs = do
    let tree = Leaf
    buildRecursive logs tree

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l msg r) = inOrder l ++ [msg] ++ inOrder r

getRelevantLogs :: [LogMessage] -> [String]
getRelevantLogs [] = []
getRelevantLogs ((LogMessage (Error severity) _ msg) : s)
    | severity >= 50 = msg : getRelevantLogs s
    | otherwise      = getRelevantLogs s
getRelevantLogs (_ : s) = getRelevantLogs s

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = getRelevantLogs (inOrder (build logs))

getNum :: String -> Maybe Int
getNum [] = Nothing
getNum w = readMaybe w :: Maybe Int

parseMessage :: String -> LogMessage
parseMessage ln = do
    let w = words ln
    let t :: (Maybe MessageType, Maybe Int, [String])
        t = case take 1 w of
            ["I"] -> (Just Info, getNum (head (drop 1 w)), drop 2 w)
            ["W"] -> (Just Warning, getNum (head (drop 1 w)), drop 2 w)
            ["E"] -> (Just (Error (fromMaybe (-1) (getNum (head (drop 1 w))))),
                      getNum (head (drop 2 w)),
                      drop 3 w)
            _ -> (Nothing, Just (-1), w)

    case t of
        (Nothing, _, _) -> Unknown ln
        (_, Nothing, _) -> Unknown ln
        (Just (Error (-1)), _, _) -> Unknown ln
        (Just typ, Just stamp, w) -> LogMessage typ stamp (unwords w)

parse :: String -> [LogMessage]
parse f = map parseMessage (lines f)
