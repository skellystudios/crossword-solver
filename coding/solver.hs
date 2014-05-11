module Solver where 

import Data.List

data Clue = DefNode String ClueTree 
  deriving Show

data ClueTree = ConsNode ClueTree ClueTree | Leaf String | AnagramNode Anagrind [String] | InsertionNode InsertionIndicator ClueTree ClueTree | HiddenWordNode HWIndicator [String]
  deriving Show

data Anagrind = AIndicator [String] deriving Show
data InsertionIndicator = IIndicator [String] deriving Show
data HWIndicator = HWIndicator [String] deriving Show


includeReversals xs = xs ++ [(snd(x),fst(x)) | x <- xs] 

twoParts xs = map (\x -> (head x, (head . tail) x)) (nPartitions 2 xs)
threeParts xs = map (\x -> (head x, (head . tail) x , (head . tail . tail) x)) (nPartitions 3 xs)

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
	++ (if length ys > 1 then makeHiddenWordNodes ys else [])
	++ (if length ys > 2 then makeInsertionNodes ys else [])


--- DISPLAY FUNCTIONS

showTree                :: (Show a) => Tree a -> String
showTree (DefNode d tree)       =  "Definition: " ++ show d ++ " \n" ++ showTree tree
showTree x   = show x



makeConsNodes :: [String] -> [ClueTree]
makeConsNodes xs = let parts = twoParts xs
                   in concat [[ConsNode x' y' |x' <- (expand (fst part)), y' <- (expand (snd part))] | part <- parts]  

-- ANAGRAMS
 
-- Sometimes need to use synonymns here ??? Maybe anagram subtypes needs to be a special type of subtree
makeAnagramNodes :: [String] -> [ClueTree]
makeAnagramNodes xs = let parts = twoParts xs
                  in [AnagramNode (AIndicator x) y | (x,y) <- parts, isAnagramWord(x)] 

isAnagramWord :: [String] -> Bool
isAnagramWord ["mixed"] = True
isAnagramWord ["shredded"] = True
isAnagramWord _ = False

anagrams :: String -> [String]
anagrams x = anagrams1 x []

anagrams1 :: String -> String -> [String]
anagrams1 [] ys = [ys]
--anagrams1 (x:[]) ys = [x:ys, ys ++ [x]]
anagrams1 (x:xs) ys = (anagrams1 xs (x:ys)) ++ (anagrams1 xs (ys++[x]))


-- INSERTIONS
makeInsertionNodes :: [String] -> [ClueTree]
makeInsertionNodes xs = let parts = threeParts xs
                  in [InsertionNode (IIndicator y) x' z' | (x,y,z) <- parts, isInsertionWord(y), x' <- (expand x), z' <- (expand z)] 


insertInto :: String -> String -> [String] -- TODO: we don't want to have insertInto 'abc' 'xyz' = abcxyz
insertInto xs [] = [xs]
insertInto xs (y:ys) = [y:(xs ++ ys)] ++ (map ((:) y) (insertInto xs ys)) 


isInsertionWord ["in"] = True
isInsertionWord _ = False


-- HIDDEN WORDS
makeHiddenWordNodes :: [String] -> [ClueTree]
makeHiddenWordNodes xs = let parts = twoParts xs
                  in [HiddenWordNode (HWIndicator x) y | (x,y) <- parts, isHWIndicator(x)] 


isHWIndicator ["found","in"] = True
isHWIndicator _ = False

substr [] = [[]]
substr (x:xs) = (map ((:) x) (substr xs)) ++ substr xs 


concatWithSpaces (x:[]) = x
concatWithSpaces (x:xs) = x ++ " " ++ concatWithSpaces xs
 
clue1 = words "companion shredded corset"
clue2 = words "notice in flying coat"

-- Now we evaluate
eval :: Clue -> [String]
eval x = let DefNode y z = x in Data.List.intersect (syn y) (eval_tree z)

eval_tree :: ClueTree -> [String]
eval_tree (AnagramNode x y) = anagrams(concat(y))
eval_tree (Leaf x) = syn x ++ [x]
eval_tree (ConsNode x y) = [x' ++ y' | x' <- eval_tree(x), y' <- eval_tree(y)]
eval_tree (InsertionNode ind x y) = concat[insertInto x' y' | x' <- eval_tree(x), y' <- eval_tree(y)]
eval_tree (HiddenWordNode ind ys) = substr (concat ys)

syn :: String -> [String]
syn "notice" = ["ack", "acknowledge", "sign"] ++  [show x | x <- [1..50]]
syn "coat" = ["jacket"]
syn "companion" = ["friend", "escort", "mate"]
syn "shredded" = ["changed", "stripped"]
syn "corset" = ["basque"]  ++  [show x | x <- [1..50]]
syn "flying" = ["jet"]  ++  [show x | x <- [1..50]]
syn _ = []




-- Cons node equivalence - write it as a list, don't allow cons node as a child
-- Memoiszation
