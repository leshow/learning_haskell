module Golf where
--unfinished
{-
	FINALLY
-}
skips :: [a] -> [[a]]
skips []		= []
skips [x]		= [[x]]
skips xs 		= map (everyN xs) [1..length xs] -- partially apply everyN with list xs
												-- [1..length xs] -> makes a list from 1 to the length of list xs, this will be our values of n for everyN
												-- map calls the partially applied everyN function onto that list

{-
	gets every nth element in a list, example everyN "ABCD" 2 = "BD"
-}
everyN:: [a] -> Int -> [a]
everyN xs n	= case drop (n-1) xs of	-- drop the first part of the list up to n-1
				[] -> []
				(z:zs) -> z : everyN zs n -- cons nth element to list, call again with remaining