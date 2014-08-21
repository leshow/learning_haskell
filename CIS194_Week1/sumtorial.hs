--define the input type and output type, along w/ 2 clauses
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

doubleUs x y = x * 2 + y * 2

-- alternate way to write the first function
sumOther :: Integer -> Integer
sumOther n = if n == 0
	then 0
	else n + sumOther (n - 1)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

hailstone :: Integer -> Integer
hailstone n
	| n `mod` 2 == 0 	= n `div` 2
	| otherwise			= 3 * n + 1

-- `mod` written as an infix function here, as prefix mod n 2
-- the backspace transforms it to infix

-- functions have arguments arg1 -> arg2 -> ... -> result
f :: Int -> Int -> Int -> Int
f x y z = x + y + z

-- lists
nums, range, range2 :: [Integer]
nums = [1,2,3,19]
range = [1..100] -- counts to 100.  '..' is the range function
range2 = [2,4..100] -- counts by 2 to 100. the comma represents the step

-- ++ concatenates lists "hel" ++ "lo"
-- : adds to begining 'h':"ello"
-- !! gets by index "hello" !! 1 = e

--list functions
-- head (first element)
-- tail (chops off list's head and returns list)
-- last (last element)
-- init (returns list without last element)
-- []  this is an empty list


hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

-- list pattern matching
intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (x:xs) = 1 + intListLength xs -- pattern matches list to be first
-- element 'x' cons the rest of the list 'xs' recursively adds it's intListLength

{-
 we can pattern match to get the first 2 elements in a list like this (x:(y:zs))
 this is equivalent to (x:y:zs) 

  _ defines a wildcare. ex. (_:xs) if we didn't care about x for intListLength
-}

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo [] 			= [] 	-- empty list case
sumEveryTwo	[x] 		= [x]	-- single element case
sumEveryTwo (x:y:zs) 	= (x + y) : sumEveryTwo zs

hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1


-- just messing around
printStrRecursive :: [Char] -> [Char]
printStrRecursive []		= ""
printStrRecursive (x:xs) 	= x : printStrRecursive xs