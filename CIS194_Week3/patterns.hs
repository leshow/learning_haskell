data IntList = Empty | Cons Int IntList
  deriving Show

addOneToAll :: IntList -> IntList
addOneToAll Empty           = Empty
addOneToAll (Cons i list)   = Cons (i+1) (addOneToAll list)

absAll :: IntList -> IntList
absAll Empty            = Empty
absAll (Cons i list)    = Cons (abs i) (absAll list)

{- etc etc... let's introduce those higher-order functions..
    unfortunately I already read ahead and used them in the last assignment.
    WOOPS!
    -}

mapToAll :: (Int -> Int) -> IntList -> IntList
mapToAll _ Empty  = Empty
mapToAll func (Cons i list)  = Cons (func i) (mapToAll func list)

keepOnlyPos :: IntList -> IntList
keepOnlyPos Empty           = Empty
keepOnlyPos (Cons i list)   =