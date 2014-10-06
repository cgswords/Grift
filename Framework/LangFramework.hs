module Framework.LangFramework where

type Var   = String
type Label = String

data Op  = Plus 
         | Minus
         | Times
         | Divide
  deriving (Show, Eq)          

data Prim = I Int
          | V Var
          | L Label
          | True
          | False
  deriving (Show, Eq)          
