import System.IO.Unsafe
import qualified Data.Map as Map
let x = unsafePerformIO . readFile $ "out_full.hs"
let y = (read x)::[([Char], [[Char]])]
let thes = Map.fromList y