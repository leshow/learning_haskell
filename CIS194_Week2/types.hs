data Thing = Shoe 
           | Ship 
           | Foo 
           | Pills 
           | King
  deriving Show

shoe :: Thing
shoe = Shoe

listOfThings :: [Thing]
listOfThings = [Shoe,Foo,King,Pills,King]

isSmall :: Thing -> Bool
isSmall Shoe 			= True
isSmall Ship 			= False
isSmall Foo 			= True
isSmall Pills 			= True
isSmall King 			= False

-- here we use a guard condition with the wildcard to write the above function shorter
isSmall2 :: Thing -> Bool
isSmall2 Ship 		= False
isSmall2 King 		= False
isSmall2 _ 			= True

data FailableDouble = Failure
					| OK Double
	deriving Show

{- OK 4.4 in ghci, 
	:type it returns FailableDouble -}

safeDiv :: Double -> Double -> FailableDouble
safeDiv	_ 0		= Failure
safeDiv x y		= OK ( x / y )

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

-- make a person type, containing their name, age, and favorite thing
data Person = Person String Int Thing
	deriving Show

brody :: Person
brody = Person "Brody" 22 Foo

eminem = Person "Eminem" 41 Pills

getAge :: Person -> Int
getAge (Person _ a _) = a