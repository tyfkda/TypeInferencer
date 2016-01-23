module Type (Type(..)) where

data Type = TInt
          | TFun Type Type
          | TVar Int
  deriving (Eq)

instance Show Type where
  show TInt        = "Int"
  show (TFun p e)  = pp p ++ " -> " ++ show e
    where pp fun@(TFun _ _) = "(" ++ show fun ++ ")"
          pp t              = show t
  show (TVar i)    = "t" ++ show i
