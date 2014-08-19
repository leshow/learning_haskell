toDigits :: Integer -> [Integer]
toDigits n
	| n <= 0		= []
	| otherwise		=  toDigits (n `div` 10) ++ [(n `mod` 10)]


toDigitsRev :: Integer -> [Integer]
toDigitsRev n
	| n <= 0		= []
	| otherwise		=  (n `mod` 10) : toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []			= []
doubleEveryOther [x]		= [x]
doubleEveryOther (x:xs)		=  x : (2 * head xs) : doubleEveryOther (tail xs)

doubleFromRight :: [Integer] -> [Integer]
doubleFromRight xs = reverse (doubleEveryOther (reverse xs))

-- the fancy way -- double from right
double :: [Integer] -> [Integer]
double = zipWith ($) (cycle [(*2),id])

sumDigits :: [Integer] -> Integer
sumDigits []		= 0
sumDigits (x:xs)	= (sum (toDigits x)) + (sumDigits xs)

validate :: Integer -> Bool
validate n
	| (sumDigits (doubleFromRight (toDigits n))) `mod` 10 == 0	= True
	| otherwise 												= False