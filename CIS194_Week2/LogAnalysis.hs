{- OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import System.IO
{-
 my first failed attempt:
parseMessage :: String -> LogMessage
parseMessage message = 
	let typeM = case (head msg) of
		"I" -> Info
		"W" -> Warning
		"E" -> Error (read (tail (head msg)))
	in LogMessage typeM stamp rest 
	where 	
		msg = words message
		stamp = read (head (tail msg))
		rest  = unwords (tail (tail msg))
parseMessage _ =  Unknown "This is not the right format"
-}
parseMessage :: String -> LogMessage
parseMessage message = case words message of
						("I":timestamp:rest) -> LogMessage Info (read timestamp) (unwords rest)
						("W":timestamp:rest) -> LogMessage Warning (read timestamp) (unwords rest)
						("E":err:timestamp:rest) -> LogMessage (Error (read err)) (read timestamp) (unwords rest)
						msg -> Unknown "Wrong log message dummy"

parse :: String -> [LogMessage]
parse content = map parseMessage $ lines content -- eq to -> map parseMessage . lines

getTimestamp :: LogMessage -> Int 
getTimestamp _															= 0
getTimestamp (LogMessage _ stamp _)					= stamp
getTimestamp (LogMessage (Error _) stamp _)	= stamp

getSeverity :: LogMessage -> Int
getSeverity _																	= 0
getSeverity (LogMessage (Error severity) _ _)	= severity

getString :: LogMessage -> String
getString (LogMessage _ _ stuff)					= stuff 
getString (LogMessage (Error _) _ stuff)	= stuff

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree	= tree 			-- if the LogMessage was const with Unknown, return original MessageTree
insert logX Leaf				= Node Leaf logX Leaf
insert logX (Node treeA logY treeB)
	|	x == y	= Node treeA logY treeB
	| y < x		= Node treeA logY (insert logX treeB)
	| y > x		= Node (insert logX treeA) logY treeB
	where
		x	= getTimestamp logX
		y = getTimestamp logY 

build :: [LogMessage] -> MessageTree
build msgs = foldr (insert) Leaf msgs	-- foldr (function) startVal list

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf	= []
inOrder	(Node t1 a t2)	= inOrder t1 ++ [a] ++ inOrder t2

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs	= map getString $ filter (\x -> (getSeverity x) > 50) (inOrder (build msgs))


{- if we wanted to make it work with foldl we could swap the arguments for insert 
insert1 :: MessageTree -> LogMessage -> MessageTree
insert1  tree	(Unknown _) = tree 			-- if the LogMessage was const with Unknown, return original MessageTree
insert1  Leaf	logX			= Node Leaf logX Leaf
insert1  (Node treeA logY treeB) logX
	|	x == y	= Node treeA logY treeB
	| y < x		= Node treeA logY (insert logX treeB)
	| y > x		= Node (insert logX treeA) logY treeB
	where
		x	= getTimestamp logX
		y = getTimestamp logY 

		-- then build could be: foldl (insert1) Leaf msgs


buildOld :: [LogMessage] -> MessageTree
buildOld []	= Leaf
buildOld (x:xs) = insert x (build xs)
-}
