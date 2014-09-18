module Golf where
--unfinished
{-
skips :: [a] -> [[a]]
skips []		= []
skips [x]		= [[x]]
skips list		= foldr (\x -> everyN (x+1)) 1 list
-}

bigList list = bigList (doList 0 list)

doList x list = everyN (x+1) list

everyN n xs	= case drop (n-1) xs of
				(z:zs) -> z : everyN n zs
				[] -> []