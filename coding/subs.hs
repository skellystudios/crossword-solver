module Main where 

remove xs ys = let n = (findIn xs ys 0 0) in if n == -1 then [] else removeFrom ys n (length xs)

removeFrom ys 0 0 = ys
removeFrom (y:ys) 0 m = removeFrom ys 0 (m-1)
removeFrom (y:ys) n m = y:(removeFrom ys (n-1) m)

findIn [] ys n f = n
findIn xs [] n f = -1
findIn (x:xs) (y:ys) n 0 = if x==y 
							then findIn xs ys n 1 
							else findIn (x:xs) (ys) (n+1) 0
findIn (x:xs) (y:ys) n 1 = if x==y 
							then findIn xs ys n 1
							else -1