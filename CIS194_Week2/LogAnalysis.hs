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
parse fileName = do  
				contents <- readFile fileName 
				parseMessage (lines contents)