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

eval_tree :: ClueTree  -> Constraints -> [String]
eval_tree (AnagramNode x y) c = 
								let y' = (concat y) in 	
								if (is_less_than_min (minC c) (length y')) then [] else  filter (is_prefix_with (prefC c)) . (delete y') . anagrams  $ y'

eval_tree (Leaf x) c = filter (fits_constraints c) (syn x ++ [x])
eval_tree (ConsListNode xs) c = eval_trees xs c --map concat (sequence (map (eval_tree n) xs))
-- eval_tree (ConsNode x y) c = [x' ++ y' | x' <- eval_tree x (noPrefix c), y' <- eval_tree y (Constraints Unconstrained (mx - length x') (mn - length x'))]
eval_tree (InsertionNode ind x y) c = concat[insertInto x' y' | y' <- eval_tree y (noMin . noPrefix $ c), x' <- eval_tree x (decreaseMax (length y') . decreaseMin (length y') . noPrefix $ c)]
eval_tree (SubtractionNode ind x y) c = concat[subtractFrom x' y' | x' <- eval_tree x no_constraints, y' <- eval_tree y no_constraints, fits_min c (length y' - length x'), fits_min c (length y' - length x')]
eval_tree (HiddenWordNode ind ys) c = [x | x <- substr (concat ys), (length x) > 0, fits_constraints c x]
eval_tree (ReversalNode ind ys) c = map reverse (eval_tree ys c)
eval_tree (FirstLetterNode ind ys) c = [firstLetter ys]
eval_tree (LastLetterNode ind ys) c = [lastLetter ys]
eval_tree (PartialNode ind y) c = filter (fits_constraints c) (concat [top_tail_substrings y | y <- eval_tree y no_constraints])
eval_tree (ConsIndicatorLeaf x) c = [""]



eval_trees :: [ClueTree] -> Constraints -> [String]
eval_trees (x:[]) c = eval_tree x (noPrefix c)
eval_trees (x:xs) (Constraints p mx mn) =
  let starts = [start | start <- eval_tree x (Constraints Unconstrained mx NoMin), fits p start]
  in  concatMap f $ starts 
  where f start =  map (\x -> start ++ x) (eval_trees xs (noMin . add_partial start $ (Constraints p mx mn)))

--no_constraints
--------- CONSTRAINTS

class Constraint c where
    fits :: c -> String -> Bool
instance Constraint MaxLength where
    fits mx s = is_lte_max mx (length s)
instance Constraint MinLength where
    fits mn s = is_gte_min mn (length s)
instance Constraint PrefixConstraint where
	fits p s = is_prefix_with p s

no_constraints = (Constraints Unconstrained NoMax NoMin)

new_constraint n = Constraints (Prefix []) (Max n) (Min n)

is_prefix_with (Prefix p) x = is_prefix (p ++ x)
is_prefix_with Unconstrained x = True

is_lte_max (Max mx) n = n <= mx 
is_lte_max NoMax n = True

is_gte_min (Min mn) n = n >= mn 
is_gte_min NoMin n = True

is_less_than_min (Min mn) n = n < mn 
is_less_than_min NoMin n = True

extend_prefix (Prefix p) x = Prefix (p ++ x)
extend_prefix Unconstrained x = Unconstrained

fits_max (Constraints p mx mn) x = is_lte_max mx x
fits_min (Constraints p mx mn) x = is_gte_min mn x
fits_constraints (Constraints p mx mn) x = (fits p x) && (fits mx x) && (fits mn x)

add_partial :: String -> Constraints -> Constraints
add_partial x (Constraints p mx mn) = decreaseMax (length x) $ (Constraints (extend_prefix p x) mx mn) 

decreaseMax :: Int -> Constraints -> Constraints
decreaseMax n (Constraints p (Max mx) mn) = Constraints p (Max (mx - n)) mn
decreaseMax n (Constraints p NoMax mn) = Constraints p NoMax mn

increaseMin :: Int -> Constraints -> Constraints
increaseMin n (Constraints p mx (Min mn)) = Constraints p mx (Min (mn + n))
increaseMin n (Constraints p mx NoMin) = Constraints p mx NoMin

decreaseMin :: Int -> Constraints -> Constraints
decreaseMin n (Constraints p mx (Min mn)) = Constraints p mx (Min (mn - n))
decreaseMin n (Constraints p mx NoMin) = Constraints p mx NoMin

maxC :: Constraints -> MaxLength
maxC (Constraints p mx mn) = mx

minC :: Constraints -> MinLength
minC (Constraints p mx mn) = mn

prefC :: Constraints -> PrefixConstraint
prefC (Constraints p mx mn) = p

noPrefix :: Constraints -> Constraints
noPrefix (Constraints p mx mn) = (Constraints Unconstrained mx mn)

noMin :: Constraints -> Constraints
noMin (Constraints p mx mn) = (Constraints p mx NoMin)

noMax :: Constraints -> Constraints
noMax (Constraints p mx mn) = (Constraints p NoMax mn)

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
evaluate = concatMap eval 

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