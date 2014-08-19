data Thing = Shoe 
           | Ship 
           | SealingWax 
           | Cabbage 
           | King
  deriving Show

  shoe :: Thing
  shoe = Shoe

  listOfThings :: [Thing]
  listOfThings = [Shoe,SealingWax,King,Cabbage,King]