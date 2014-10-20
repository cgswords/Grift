module Framework.L0 where

import Framework.LangFramework

data Program
      = Prog [Func] Main

data Main = 
      Main [Block]

data Func 
      = Func Name [Var] [Block]

data Block = Block Label [Expr] | EmptyBlock [Expr]

data Expr 
      = SetP Var Prim
      | SetB Var Bop
      | SetC Var Call
      | Begin [Expr]
      | Ret Prim
      | Jump Prim
      | Branch Bop Prim Prim
      | CallFunc Call
  deriving (Show, Eq)

data Call
      = FuncCall Prim [Prim]
  deriving (Show, Eq)


data Bop 
      = Binop Op Prim Prim
  deriving (Show, Eq)
