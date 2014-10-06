module Compiler.Emit (emit) where

import Framework.LangFramework
import qualified Framework.LOut as L
import Data.List

emitProgram :: L.Expr -> String
emitProgram prog = 
  "define i64 @main() nounwind {\n" ++ 
    emit prog ++ 
  "\n} "

emit :: L.Expr -> String
emit (L.SetP var p)                  = emitSet (emitVar var) (emitPrim p) 
emit (L.SetB var (L.Binop op p1 p2)) = emitSet (emitVar var) 
                                               (emitBinop (emitOp op) (emitPrim p1) (emitPrim p2))
emit (L.Begin es)                    = foldr (++) " " $ intersperse "\n" $ map emit es
emit (L.Ret e)                       = emitReturn $ emitPrim e

emitOp :: Op -> String
emitOp Plus   = "add i64"
emitOp Times  = "mul i64"
emitOp Divide = "div i64"
emitOp Minus  = "sub i64"

emitPrim :: Prim -> String
emitPrim (I i) = (show i) -- "(i64 " ++ (show i) ++ ")"
emitPrim (V v) = emitVar v -- "(i64 " ++ (show i) ++ ")"

emitVar :: Var -> String
emitVar = ("%" ++) 

emitBinop :: String -> String -> String -> String
emitBinop op e1 e2 = op ++ " " ++ e1 ++ ", " ++ e2

emitSet :: String -> String -> String
emitSet var op     = "    " ++ var ++ " = " ++ op

emitReturn :: String -> String
emitReturn s = "    ret i64 " ++ s

