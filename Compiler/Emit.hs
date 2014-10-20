module Compiler.Emit (emit) where

import Framework.LangFramework
import Framework.L0
import Data.List

emit :: Program -> String
emit (Prog fs main) = foldr 
                       (\ x s -> emitFunction x ++ "\n" ++ s) 
                       (emitMain main)
                       fs

emitMain :: Main -> String
emitMain (Main blocks) = "define i64 @main() nounwind {\n" ++ 
                         (foldr (\x s -> emitBlock x ++ "\n" ++ s) "" blocks) ++
                         "\n} "

emitFunction :: Func -> String
emitFunction (Func name args blocks) = "define i64 @" ++ name ++ "(" ++ (emitArgs args) ++ ") {\n" ++
                                       (foldr (\x s -> emitBlock x ++ "\n" ++ s)
                                              "}\n"
                                              blocks)

emitArgs :: [Var] -> String
emitArgs (a:args) = foldl'
                      (\ s v -> s ++ (", i64 " ++ emitVar v) )
                      ("i64 " ++ emitVar a)
                      args

emitBlock :: Block -> String
emitBlock (Block l es)    = "  " ++ l ++ ":\n" ++ (foldr (\ e s -> emitExpr e ++ "\n" ++ s) "" es)
emitBlock (EmptyBlock es) = foldr (\ e s -> emitExpr e ++ "\n" ++ s) "" es

emitExpr :: Expr -> String
emitExpr (SetP var p)                = emitSet (emitVar var) (emitPrim p) 
emitExpr (SetB var (Binop op p1 p2)) = emitSet (emitVar var) 
                                               (emitBinop (emitOp op) (emitPrim p1) (emitPrim p2))
emitExpr (Begin es)                  = foldr (++) " " $ intersperse "\n" $ map emitExpr es
emitExpr (Ret e)                     = emitReturn e
emitExpr (CallFunc c)                = emitCall c

emitCall :: Call -> String
emitCall (FuncCall name args) =  "    call i64 " ++ 
                                 (emitPrim name) ++ 
                                 "(" ++ (emitCallArgs args) ++ ")"

emitCallArgs :: [Prim] -> String
emitCallArgs (a:args) = foldl'
                          (\ s v -> s ++ (", i64 " ++ emitPrim v) )
                          ("i64 " ++ emitPrim a)
                          args

emitOp :: Op -> String
emitOp Plus   = "add i64"
emitOp Times  = "mul i64"
emitOp Divide = "div i64"
emitOp Minus  = "sub i64"

emitPrim :: Prim -> String
emitPrim (I i)  = (show i) -- "(i64 " ++ (show i) ++ ")"
emitPrim (V v)  = emitVar v -- "(i64 " ++ (show i) ++ ")"
emitPrim (Fn f) = "@" ++ f
emitPrim Void   = "void"

emitVar :: Var -> String
emitVar = ("%" ++) 

emitBinop :: String -> String -> String -> String
emitBinop op e1 e2 = op ++ " " ++ e1 ++ ", " ++ e2

emitSet :: String -> String -> String
emitSet var op     = "    " ++ var ++ " = " ++ op

emitReturn :: Prim -> String
emitReturn Void  = "    ret void"
emitReturn p = "    ret i64 " ++ (emitPrim p)


