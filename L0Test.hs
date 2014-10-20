module L0Test where

import Framework.L0
import qualified Framework.LangFramework as LF

buildProg fns main = Prog fns $ Main [EmptyBlock main]

test1 = buildProg [] $ [Ret (LF.I 1)]

test2 = buildProg [] $ [Begin
          [SetB "tmp1" (Binop LF.Plus (LF.I 1) (LF.I 1)),
           SetB "tmp2" (Binop LF.Plus (LF.I 1) (LF.I 1))]]

test3 = buildProg [] $ [Begin
          [SetP "tmp1" (LF.I 1),
           SetB "tmp2" (Binop LF.Times (LF.V "tmp1") (LF.I 2)),
           SetB "tmp3" (Binop LF.Times (LF.V "tmp2") (LF.I 3)),
           SetB "tmp4" (Binop LF.Times (LF.V "tmp3") (LF.I 4)),
           SetB "tmp5" (Binop LF.Times (LF.V "tmp4") (LF.I 5)),
           Ret $ LF.V "tmp5"]]

test4 = 
  buildProg [even] $ [Begin
                       [CallFunc $ FuncCall (LF.Fn "even") [LF.I 5],
                        Ret $ LF.I 0]]
  where
    even = Func "even" ["v"] 
                       [Block "entry" [Ret $ LF.I 0]]

