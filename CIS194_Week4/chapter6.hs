data Color = Red | Green | Purple
  deriving Show

colorEq :: Color -> Color -> Bool
colorEq Red Red       = True
colorEq Green Green   = True
colorEq Purple Purple = True
colorEq _ _           = False
