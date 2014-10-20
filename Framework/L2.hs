module Framework.L2 where

import Framework.LangFramework

data Expr 
      = Letrec [(Label, [Var], Body)] Body
  deriving (Show, Eq)

data Body
      = Locals [Var] Tail

data Tail
      = TIf Pred Tail Tail
      | TBegin [Effect] Tail
      | TApp [Prim]
      | TBinop Prim Prim
      | TPrim Prim
  deriving (Show, Eq)
  
data Pred 
      = PPrim Prim
      | PBinop Bop
      | PIf Pred Pred Pred
      | PBegin [Effect] Pred
  deriving (Show, Eq)

data Effect 
      = Nop
      | SetP Var Prim
      | SetB Var Bop
      | SetA Var [Prim]
      | EApp [Prim]
      | EIf Pred Effect Effect
      | Begin [Effect] Effect
  deriving (Show, Eq)

data Bop 
      = Binop Op Prim Prim
  deriving (Show, Eq)
