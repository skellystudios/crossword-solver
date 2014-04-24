data ClueTree = DefNode String ClueTree | ConsNode ClueTree ClueTree | Leaf String | AnagramNode String
 deriving Show

includeReversals xs = xs ++ [(snd(x),fst(x)) | x <- xs] 

twoParts xs = map (\x -> (head x, (head . tail) x)) (nPartitions 2 xs)

nPartitions :: Int -> ([String] -> [[[String]]])
nPartitions n xs = [xss | xss <- partitions xs, length xss == n]


partitions [] = [[]]
partitions (x:xs) = [[x]:p | p <- partitions xs] ++ [(x:ys):yss | (ys:yss) <- partitions xs]


makeDefs :: [String] -> [ClueTree]
makeDefs xs = let parts = twoParts xs
			  in concat [[DefNode (concat (fst part)) y' | y' <- (expand (snd part))] | part <- parts]


item2 x = head (tail x)