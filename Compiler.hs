module Compiler where

import Compiler.Emit
import qualified L0Test as L0T

import System.IO.Unsafe

test1 = unsafePerformIO $ putStrLn $ emit L0T.test1
test2 = unsafePerformIO $ putStrLn $ emit L0T.test2
test3 = unsafePerformIO $ putStrLn $ emit L0T.test3
test4 = unsafePerformIO $ putStrLn $ emit L0T.test4

