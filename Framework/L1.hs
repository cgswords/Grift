module Framework.LIf where

import Framework.LangFramework

data Expr 
      = Letrec [(Label, [Var], Tail)] Tail
  deriving (Show, Eq)

data Tail
      = TIf Pred Tail Tail
      | TBegin [Effect] Tail
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
      | EIf Pred Effect Effect
      | Begin [Effect] Effect
  deriving (Show, Eq)

data Bop 
      = Binop Op Prim Prim
  deriving (Show, Eq)
