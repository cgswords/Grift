module Test where

import qualified Framework.LOut as LOut
import qualified Framework.LangFramework as LF

test1 = LOut.Ret (LF.I 1)

test2 = LOut.Begin
          [LOut.SetB "tmp1" (LOut.Binop LF.Plus (LF.I 1) (LF.I 1)),
           LOut.SetB "tmp2" (LOut.Binop LF.Plus (LF.I 1) (LF.I 1))]

test3 = LOut.Begin
          [LOut.SetP "tmp1" (LF.I 1),
           LOut.SetB "tmp2" (LOut.Binop LF.Times (LF.V "tmp1") (LF.I 2)),
           LOut.SetB "tmp3" (LOut.Binop LF.Times (LF.V "tmp2") (LF.I 3)),
           LOut.SetB "tmp4" (LOut.Binop LF.Times (LF.V "tmp3") (LF.I 4)),
           LOut.SetB "tmp5" (LOut.Binop LF.Times (LF.V "tmp4") (LF.I 5)),
           LOut.Ret $ LF.V "tmp5"]

