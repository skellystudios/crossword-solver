module TestSystemCall () where

import System.IO.Unsafe
import System.Process


a  = unsafePerformIO . system $ "pwd"


