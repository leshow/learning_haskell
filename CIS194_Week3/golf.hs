module Golf where
{-
	FINALLY -- Exercise 1
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

{- Exercise 2 
n takes our triplet and returns true if the middle element is larger than both
the lambda passed to map takes the middle element of the triplet and returns it
the zip3 takes the list, a list starting at the second elem, and a list starting
	at the third element. and creates a list of triplets with each of those values
	so [1,2,3,4]
	becomes: [(1,2,3),(2,3,4)]
-}		
localMaxima:: [Integer] -> [Integer]
localMaxima list =  map (\(q,w,e) -> w) $ filter n $ zip3 list (tail list) (tail $ tail list)
	where 
		n (j,k,l) = (k > j) && (k > l)