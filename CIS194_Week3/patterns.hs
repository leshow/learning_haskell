data IntList = Empty | Cons Int IntList
  deriving Show

addOneToAll :: IntList -> IntList
addOneToAll Empty					= Empty
addOneToAll (Cons i list)	= Cons (i+1) (addOneToAll list)

absAll :: IntList -> IntList
absAll Empty							= Empty
absAll (Cons i list)			= Cons (abs i) (absAll list)

{- etc etc... let's introduce those higher-order functions.. 
	unfortunately I already read ahead and used them in the last assignment.
	WOOPS!
	-}