module Golf where
--unfinished
skips :: [a] -> [[a]]
skips []		= []
skips [x]		= [x]
skips list		= 

everyN n xs	= case drop (n-1) xs of
				(z:zs) -> z : everyN n zs
				[] -> []