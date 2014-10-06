module Framework.Util where

import Framework.LangFramework
import Control.Monad.State

incr :: State Int Int
incr = do i <- get
          put (i+1)
          return i

uniqueLabel :: String -> State Int Label
uniqueLabel s = do i <- incr
                   return $ s ++ (show i)
     


