module Compiler.ExposeBasicBlocks (exposeBasicBlocks) where

import Framework.LangFramework
import qualified Framework.LangFramework as LF
import Framework.Util
import Framework.LIf
import Control.Monad.State
import Control.Applicative

import Data.Foldable
import Data.List hiding (tail)
import Prelude hiding (pred,tail)

exposeBasicBlocks :: Expr -> State Int Expr
exposeBasicBlocks = expr

type Bindings = [(Label, Tail)]

expr :: Expr -> State Int Expr
expr (Letrec bindings body) = 
  do (bodies, newBlocks) <- foldrM (\ (lbl,t) (bodies, blocks) -> 
                                      do (body, newBlocks) <- tail t
                                         let bodies' = (lbl,body):bodies
                                             blocks' = newBlocks ++ blocks
                                         return (bodies', blocks'))
                                   ([],[]) bindings
     (bod, blocks)       <- tail body                             
     return $ Letrec (bodies ++ newBlocks ++ blocks) bod

tail :: Tail -> State Int (Tail, Bindings)
tail t@(TPrim prim)     = return (t, []) 
tail (TBegin es t)      = do (tbody, tbindings) <- tail t
                             (ebody, ebindings) <- effects (reverse es) (TBegin [] tbody) []
                             return (ebody, ebindings ++ tbindings)
tail (TIf test con alt) = do conLbl           <- uniqueLabel "true"
                             altLbl           <- uniqueLabel "false"
                             (conBody, cbind) <- tail con
                             (altBody, abind) <- tail alt
                             (pBody,   pbind) <- pred test conLbl altLbl
                             let conBind      = (conLbl, conBody)
                                 altBind      = (altLbl, altBody)
                             return (pBody, 
                                     pbind ++
                                     [conBind] ++ cbind ++
                                     [altBind] ++ abind)


effCons :: Effect -> Tail -> Tail
effCons e (TBegin es t) = TBegin (e:es) t
effCons e t             = error $ "Can't cons onto a non-begin tail"

effects :: [Effect] -> Tail -> [(Label, Tail)] -> State Int (Tail, Bindings)
effects []                      tail binds = return $ (tail, binds)
effects (Nop:es)                tail binds = effects es tail binds
effects (s@(SetP _ _):es)       tail binds = effects es (s `effCons` tail) binds
effects (s@(SetB _ _):es)       tail binds = effects es (s `effCons` tail) binds 
effects ((Begin es' e):es)      tail binds = effects (e:(reverse es' ++ es)) tail binds
effects ((EIf test con alt):es) tail binds = 
  do conLbl <- uniqueLabel "true"
     altLbl <- uniqueLabel "false"
     jmpLbl <- uniqueLabel "jump"
     let jmpTail          = (TBegin [] $ TPrim $ L jmpLbl)
     (conBody, cbind)    <- effects [con] jmpTail []
     (altBody, abind)    <- effects [alt] jmpTail []
     (pBody  , pbind)    <- pred test conLbl altLbl
     let conBind          = (conLbl, conBody)
         altBind          = (altLbl, altBody)
         jumpBind         = (jmpLbl, tail)
     effects es (TBegin [] pBody) 
                (pbind ++ 
                 [conBind] ++ cbind ++ 
                 [altBind] ++ abind ++ 
                 [jumpBind] ++ binds)  


pred :: Pred -> Label -> Label -> State Int (Tail, Bindings)
pred (PPrim LF.True)       conLbl altLbl = return $ (TPrim $ L conLbl, [])
pred (PPrim LF.False)      conLbl altLbl = return $ (TPrim $ L altLbl, [])
pred p@(PBinop bop)     conLbl altLbl = return $ (TIf p (TPrim $ L conLbl) (TPrim $ L altLbl), []) 
pred (PBegin es p)      conLbl altLbl = do (pTail, predBinds) <- pred p conLbl altLbl
                                           effects (reverse es) pTail predBinds
pred (PIf test con alt) conLbl altLbl = 
  do newConLbl        <- uniqueLabel "true"
     newAltLbl        <- uniqueLabel "false"
     (conBody, cbind) <- pred con conLbl altLbl
     (altBody, abind) <- pred alt conLbl altLbl
     (pBody,   pbind) <- pred test newConLbl newAltLbl
     let conBind       = (newConLbl, conBody)
         altBind       = (newAltLbl, altBody)
     return $ (pBody,
               pbind ++ 
               [conBind] ++ cbind ++
               [altBind] ++ abind)         
  

