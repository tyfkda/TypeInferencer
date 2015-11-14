module Node (Expr(..)) where

data Expr = Natural Integer  -- integer:   value
          | Var String       -- variable:  name
          | Fun String Expr  -- function:  parameter, body
          | App Expr Expr    -- apply:     function, argument
  deriving (Show)
