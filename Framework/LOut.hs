module Framework.LOut where

import Framework.LangFramework

data Expr 
      = SetP Var Prim
      | SetB Var Bop
      | Begin [Expr]
      | Ret Prim
      | Jump Prim
      | Branch Bop Prim Prim
  deriving (Show, Eq)

data Bop 
      = Binop Op Prim Prim
  deriving (Show, Eq)
