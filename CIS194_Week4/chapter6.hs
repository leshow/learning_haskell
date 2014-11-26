--Chapter 6 from online text Real World Haskell

--imagine there were no == operator
data Color = Red | Green | Purple
  --deriving Show

colorEq :: Color -> Color -> Bool
colorEq Red Red       = True
colorEq Green Green   = True
colorEq Purple Purple = True
colorEq _ _           = False

-- it would be tedious to define equality for every type, instead
-- haskell language would implement a generic equality test
-- we use typeclasses to do this:

-- although it uses the 'class' keyword a typeclass is more similar to
-- a interface (from java) than a class from OO languages
-- a class is a set of types with a common interface
class BasicEq a where
  isEqual :: a -> a -> Bool

instance BasicEq Bool where
  isEqual True  True  = True
  isEqual False False = True
  isEqual _     _     = False


-- in the following we define a class where, when implemented we need to provide
-- a definition for at least one of the functions.
class BasicEq3 a where
  isEqual3 :: a -> a -> Bool
  isEqual3 x y = not (isNotEqual3 x y)

  isNotEqual3 :: a -> a -> Bool
  isNotEqual3 x y = not (isEqual3 x y)

-- now lets make color a part of the BasicEq3 class

instance BasicEq3 Color where
  isEqual3 Red Red        = True
  isEqual3 Green Green    = True
  isEqual3 Purple Purple  = True
  isEqual3 _ _            = False

instance Show Color where
  show Green  = "Green"
  show Purple = "Purple"
  show Red    = "Red"
-- show class has one function show that you implement to say how
-- you want your type to look as a string
-- i'm pretty sure the 'deriving Show' does the basic conversion automagically


-- simple example of doing a read and a show
main = do
        putStrLn "Please enter a double:"
        inpStr <- getLine
        let inpDouble = (read inpStr)::Double
        putStrLn ("Twice " ++ show inpDouble ++ " is " ++ show (inpDouble * 2))

--
instance Read Color where
  readsPrec _ value = tryParse [("Red",Red),("Green",Green),("Purple",Purple)]
    where tryParse [] = []
          tryParse ((attempt, result):xs) =
            if (take (length attempt) value) == attempt
              then [(result, drop (length attempt) value)]
              else tryParse xs
