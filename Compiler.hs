module Compiler where

import Compiler.Emit
import qualified Test as T

import System.IO.Unsafe

test1 = unsafePerformIO $ putStrLn $ emitProgram T.test1
test2 = unsafePerformIO $ putStrLn $ emitProgram T.test2
test3 = unsafePerformIO $ putStrLn $ emitProgram T.test3

