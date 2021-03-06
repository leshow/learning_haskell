fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = foldl (*) 1 . map (subtract 2) . filter (even)

fun2' :: Integer -> Integer
fun2' x = sum (filter (even)  (takeWhile (/=1) (iterate (list) x)))
  where
    list n = if even n
          then n `div` 2
          else 3 * n + 1

fun2'' :: Integer -> Integer
fun2'' = sum . filter (even)  . takeWhile (/=1) . iterate (list)
  where
    list n = if even n
      then n `div` 2
      else 3 * n + 1


-- Exercise 2
