module Solver where 

import Data.List

data Clue = DefNode String ClueTree 
  deriving Show

data ClueTree = ConsNode ClueTree ClueTree | Leaf String | AnagramNode Anagrind [String]
  deriving Show

data Anagrind = Indicator [String]
 deriving Show

includeReversals xs = xs ++ [(snd(x),fst(x)) | x <- xs] 

twoParts xs = map (\x -> (head x, (head . tail) x)) (nPartitions 2 xs)

nPartitions :: Int -> ([String] -> [[[String]]])
nPartitions n xs = [xss | xss <- partitions xs, length xss == n]


partitions [] = [[]]
partitions (x:xs) = [[x]:p | p <- partitions xs] ++ [(x:ys):yss | (ys:yss) <- partitions xs]


makeTree = (makeDefs . words) 

makeDefs :: [String] -> [Clue]
makeDefs xs = let parts = twoParts xs
			  in concat [[DefNode (concatWithSpaces (fst part)) y' | y' <- (expand (snd part))] | part <- includeReversals (parts)]

expand :: [String] -> [ClueTree]
expand ys = [Leaf (concatWithSpaces ys)] 
	++ (if length ys > 1 then makeAnagramNodes ys else [] )
	++ (if length ys > 1 then makeConsNodes ys else [])


makeConsNodes :: [String] -> [ClueTree]
makeConsNodes xs = let parts = twoParts xs
                   in concat [[ConsNode x' y' |x' <- (expand (fst part)), y' <- (expand (snd part))] | part <- parts]  


-- Sometimes need to use synonymns here
makeAnagramNodes :: [String] -> [ClueTree]
makeAnagramNodes xs = let parts = twoParts xs
                  in [AnagramNode (Indicator x) y | (x,y) <- parts, isAnagramWord(x)] 

item2 x = head (tail x)

isAnagramWord :: [String] -> Bool
isAnagramWord ["mixed"] = True
isAnagramWord ["shredded"] = True
isAnagramWord _ = False

concatWithSpaces (x:[]) = x
concatWithSpaces (x:xs) = x ++ " " ++ concatWithSpaces xs
 
clue1 = words "companion shredded corset"

-- Now we evaluate
eval :: Clue -> [String]
eval x = let DefNode y z = x in Data.List.intersect (syn y) (eval_tree z)

eval_tree :: ClueTree -> [String]
eval_tree (AnagramNode x y) = anagrams(concat(y))
eval_tree (Leaf x) = syn x
eval_tree (ConsNode x y) = [x' ++ y' | x' <- eval_tree(x), y' <- eval_tree(y)]

syn :: String -> [String]
syn "companion" = ["friend", "escort"]
syn "shredded" = ["changed", "stripped"]
syn "corset" = ["basque"]
syn _ = []

anagrams :: String -> [String]
anagrams x = anagrams1 x []

anagrams1 :: String -> String -> [String]
anagrams1 [] ys = [ys]
--anagrams1 (x:[]) ys = [x:ys, ys ++ [x]]
anagrams1 (x:xs) ys = (anagrams1 xs (x:ys)) ++ (anagrams1 xs (ys++[x]))


