data Thing = Shoe 
           | Ship 
           | Foo 
           | Pills 
           | King
  deriving Show
-- read | as "or"
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


type Name = String -- define a synonym for string to be used with Person constructor
-- make a person type, containing their name, age, and favorite thing
data Person = Person Name Int Thing
	deriving Show

brody :: Person
brody = Person "Brody" 22 Foo

eminem = Person "Eminem" 41 Pills

getAge :: Person -> Int
getAge (Person _ a _) = a

--pattern matching example. show behaves sort of like a toString
printName :: Person -> Name
printName x@(Person name _ _) = "Our type (" ++ show x ++ ") our name " ++ name

checkForFoo :: Person -> Bool
checkForFoo (Person _ _ Foo) 	= True
checkForFoo (Person _ _ _)		= False

-- failureToZero written using case, without the syntactic sugar
failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
	Failure -> 0
	OK d 	-> d

-- Recursive types
data IntList = Empty 
			| Cons Int IntList

intListProd :: IntList -> Int
intListProd Empty			= 1
intListProd (Cons ft sd)	= ft * intListProd sd

-- binary tree example
data Tree = Leaf Char
	|	Node Tree Int Tree

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))

-- from realworldhaskell book
type CustomerID = Int
type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                | CashOnDelivery
                | Invoice CustomerID
  deriving (Show)

getCardNumber :: BillingInfo -> CardNumber
getCardNumber	(CreditCard number _ _)	=	number
getCardNumber (CashOnDelivery)				=	"No card number"

{- writing it the way above and manually writing getter methods
	can be tedious, so you can write it using RECORD syntax: -}
data Customer = Customer {
    customerID      :: CustomerID
  , customerName    :: String
  , customerAddress :: Address
  } deriving (Show)

-- this is equal to:
{-
		data Customer = Customer Int String [String]
		            deriving (Show)

		customerID :: Customer -> Int
		customerID (Customer id _ _) = id

		customerName :: Customer -> String
		customerName (Customer _ name _) = name

		customerAddress :: Customer -> [String]
		customerAddress (Customer _ _ address) = address

-}
--Record syntax is good for large structures 