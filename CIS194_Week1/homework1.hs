toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0        = []
    | otherwise     =  toDigits (n `div` 10) ++ [(n `mod` 10)]


toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0        = []
    | otherwise     =  (n `mod` 10) : toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []         = []
doubleEveryOther [x]        = [x]
doubleEveryOther (x:xs)     =  x : (2 * head xs) : doubleEveryOther (tail xs)

doubleFromRight :: [Integer] -> [Integer]
doubleFromRight xs = reverse (doubleEveryOther (reverse xs))

-- the fancy way -- double from right
double :: [Integer] -> [Integer]
double = zipWith ($) (cycle [(*2),id])

sumDigits :: [Integer] -> Integer
sumDigits []        = 0
sumDigits (x:xs)    = (sum (toDigits x)) + (sumDigits xs)

validate :: Integer -> Bool
validate n
    | (sumDigits (doubleFromRight (toDigits n))) `mod` 10 == 0  = True
    | otherwise                                                 = False

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _   = []
hanoi n a b c   = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

{-
    3 peg soln:
        move a to b
        move a to c
        move b to c
        move a to b
        move c to a
        move c to b
        move a to b
    -}