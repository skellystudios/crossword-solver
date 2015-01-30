import Data.Array.IArray

a :: Array Integer String
a = listArray (0,2) ["a","b","c"]

syn "const" = [a!1, a!2]