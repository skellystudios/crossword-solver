module Evaluation where

import Data.Char
import Data.Binary
import Data.List 
import qualified Data.Set

import Utils
import Types
import Dictionary
import Wordlists

-- Now we evaluate
eval :: Parse -> [Answer]
eval (DefNode y z n) = let constraints = (n, n) in [Answer x (DefNode y z n) | x <- eval_tree z (Constraints (Prefix []) (Max n) (Min n))] 

eval_tree :: ClueTree  -> EvalConstraints -> [String]
eval_tree (AnagramNode x y) c = 
								let y' = (concat y) in 	
								if (is_less_than_min (minC c) length y') then [] else  filter (is_prefix_with (prefC c)) . (delete y') . anagrams  $ y'

eval_tree (Leaf x) c = filter (fits c) (syn x ++ [x])
eval_tree (ConsListNode xs) c = eval_trees (minC c) xs (Prefix []) --map concat (sequence (map (eval_tree n) xs))
{- eval_tree (ConsNode x y) c = [x' ++ y' | x' <- eval_tree x (noPrefix c), y' <- eval_tree y (Constraints Unconstrained (mx - length x') (mn - length x'))]
eval_tree (InsertionNode ind x y) (Constraints p mx mn) = concat[insertInto x' y' | x' <- eval_tree x, y' <- eval_tree mn y, length y' == (mn - length x')]
eval_tree (SubtractionNode ind x y) = concat[subtractFrom x' y' | x' <- eval_tree 99 x, y' <- eval_tree 99 y, length y' - length x' == n  ]
eval_tree (HiddenWordNode ind ys) = [x | x <- substr (concat ys), (length x) > 0, (length x) <= n, isInWordlist(x)]
eval_tree (ReversalNode ind ys) = map reverse (eval_tree n ys)
eval_tree (FirstLetterNode ind ys) = [firstLetter ys]
eval_tree (LastLetterNode ind ys) = [lastLetter ys]
eval_tree (PartialNode ind y) = concat [top_tail_substrings y | y <- eval_tree n y]
eval_tree (ConsIndicatorLeaf x) = [""]
-}


eval_trees :: Int -> [ClueTree] -> PrefixConstraint -> [String]
eval_trees n (c:[]) _ = eval_tree c (Constraints Unconstrained (Max n) (Min n))
eval_trees n (c:clues_left) p =
  let starts = [start | start <- eval_tree c (Constraints Unconstrained (Max n) NoMin), is_prefix_with p start]
  in filter is_prefix . concatMap f $ starts 
  where f start =  map (\x -> start ++ x) (eval_trees (n - (length start)) clues_left (extend_prefix p start))


--------- CONSTRAINTS

class Constraint c where
    fits :: c -> String -> Bool
instance Constraint MaxLength where
    fits mx s = is_lte_max mx (length s)
instance Constraint PrefixConstraint where
	fits p s = is_prefix_with p s


is_prefix_with (Prefix p) x = is_prefix (p ++ x)
is_prefix_with Unconstrained x = True

is_lte_max (Max mx) n = n <= mx 
is_lte_max NoMax n = True

is_less_than_min (Min mn) n = n < mn 
is_less_than_min NoMin n = True

extend_prefix (Prefix p) x = Prefix (p ++ x)
extend_prefix Unconstrained x = Unconstrained

fits_Constraints (Constraints p mx mn) x = (fits p x) && (fits mx x) --x && fits mn x

decreaseMax :: EvalConstraints -> Int -> EvalConstraints
decreaseMax (Constraints p (Max mx) mn) n = Constraints p (Max (mx - n)) mn
decreaseMax (Constraints p NoMax mn) n = Constraints p NoMax mn

increaseMin :: EvalConstraints -> Int -> EvalConstraints
increaseMin (Constraints p mx (Min mn)) n = Constraints p mx (Min (mn-n))
increaseMin (Constraints p mx NoMin) n = Constraints p mx NoMin

maxC :: EvalConstraints -> Int
maxC (Constraints p (Max mx) (Min mn)) = mx

minC :: EvalConstraints -> Int
minC (Constraints p (Max mx) (Min mn)) = mn

prefC :: EvalConstraints -> PrefixConstraint
prefC (Constraints p mx mn) = p

noPrefix :: EvalConstraints -> EvalConstraints
noPrefix (Constraints p mx mn) = (Constraints Unconstrained mx mn)

{-fits :: a -> String -> Bool
fits (Max n) s = True
fits (Prefix p) s = True
-}

{- instance Ord MinLength where
	(<) (Min n) = (<) n
	(<) NoMin = True
	(>) NoMin = True
	(>) (Min n) = (>) n
	-}



evaluate :: [Parse] -> [Answer]
evaluate = concat . (map eval) 

anagrams :: String -> [String]
anagrams [] = [[]]
anagrams xs = [x:ys | x<- nub xs, ys <- anagrams $ delete x xs]

insertInto :: String -> String -> [String] 
insertInto xs [] = [xs]
insertInto xs (y:ys) = [y:(xs ++ ys)] ++ (map ((:) y) (insertInto xs ys)) 

subtractFrom :: String -> String -> [String] 
subtractFrom xs ys = let n = (find_in xs ys 0 0) in if n == -1 then [] else [remove_from ys n (length xs)]

remove_from ys 0 0 = ys
remove_from (y:ys) 0 m = remove_from ys 0 (m-1)
remove_from (y:ys) n m = y:(remove_from ys (n-1) m)

find_in [] ys n f = n
find_in xs [] n f = -1
find_in (x:xs) (y:ys) n 0 = if x==y 
              then find_in xs ys n 1 
              else find_in (x:xs) (ys) (n+1) 0
find_in (x:xs) (y:ys) n 1 = if x==y 
              then find_in xs ys n 1
              else -1




substr [] = [[]]
substr (x:xs) = (map ((:) x) (contiguoussubstr xs)) ++ substr xs 

contiguoussubstr [] = [[]]
contiguoussubstr (x:xs) = [[x]] ++ (map ((:) x) (contiguoussubstr xs))

firstLetter = map head

lastLetter = map last

top_tail_substrings :: String -> [String]
top_tail_substrings x =  top_substrings x ++ tail_substrings x

top_substrings :: String -> [String]
top_substrings (x:[]) = []  
top_substrings (x:xs) = [[x]] ++ (map  (\y -> [x] ++ y) (top_substrings xs))


tail_substrings :: String -> [String]
tail_substrings = (map reverse) . top_substrings . reverse

{-
check_eval :: Parse -> [Answer]
-- check_eval x = let DefNode y z n = x in Data.List.intersect (syn y) ((eval_tree n z))
check_eval (DefNode y z n) = map (\x -> Answer x (DefNode y z n)) (Data.Set.toList (Data.Set.intersection wordlist_extended (Data.Set.fromList (eval_tree n z))))
-} 