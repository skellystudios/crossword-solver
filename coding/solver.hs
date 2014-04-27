module Solver where 

data ClueTree = DefNode String ClueTree | ConsNode ClueTree ClueTree | Leaf String | AnagramNode String
 deriving Show

includeReversals xs = xs ++ [(snd(x),fst(x)) | x <- xs] 

twoParts xs = map (\x -> (head x, (head . tail) x)) (nPartitions 2 xs)

nPartitions :: Int -> ([String] -> [[[String]]])
nPartitions n xs = [xss | xss <- partitions xs, length xss == n]


partitions [] = [[]]
partitions (x:xs) = [[x]:p | p <- partitions xs] ++ [(x:ys):yss | (ys:yss) <- partitions xs]


makeTree = (makeDefs . words) 

makeDefs :: [String] -> [ClueTree]
makeDefs xs = let parts = twoParts xs
			  in concat [[DefNode (concatWithSpaces (fst part)) y' | y' <- (expand (snd part))] | part <- includeReversals (parts)]

expand :: [String] -> [ClueTree]
expand ys = [Leaf (concatWithSpaces ys)] 
	++ (if length ys > 1 then makeAngaramNodes ys else [] )
	++ (if length ys > 1 then makeConsNodes ys else [])


makeConsNodes :: [String] -> [ClueTree]
makeConsNodes xs = let parts = twoParts xs
                   in concat [[ConsNode x' y' |x' <- (expand (fst part)), y' <- (expand (snd part))] | part <- parts]  

makeAngaramNodes :: [String] -> [ClueTree]
makeAngaramNodes xs = let parts = twoParts xs
                   in concat [[AnagramNode y' | x' <- (fst part), y' <- (snd part)] | part <- parts]  

item2 x = head (tail x)

isAnagramWord :: String -> Bool
isAnagramWord "mixed" = True

concatWithSpaces (x:[]) = x
concatWithSpaces (x:xs) = x ++ " " ++ concatWithSpaces xs
 