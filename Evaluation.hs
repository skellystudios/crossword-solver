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
eval (DefNode y z n) 
  = let constraints = (n, n) in [Answer x (DefNode y z n) | x <- evalTree z (Constraints (Prefix []) (Max n) (Min n))] 

evalTree :: ParseTree  -> EvalConstraints -> [String]
evalTree (AnagramNode x y) c 
  = let y' = (concat y) in 	
								if (is_less_than_min (minC c) (length y')) then [] else  filter (is_prefix_with (prefC c)) . (delete y') . anagrams  $ y'

evalTree (SynonymNode x) c 
  = filter (fits_constraints c) (synonyms x ++ [x])
evalTree (ConcatNode xs) c 
  = evalTrees xs c --map concat (sequence (map (evalTree n) xs))
-- evalTree (ConsNode x y) c = [x' ++ y' | x' <- evalTree x (noPrefix c), y' <- evalTree y (Constraints NoPrefix (mx - length x') (mn - length x'))]
evalTree (InsertionNode ind x y) c 
  = concat[insertInto x' y' | y' <- evalTree y (noMin . noPrefix $ c), x' <- evalTree x (decreaseMax (length y') . decreaseMin (length y') . noPrefix $ c)]
evalTree (SubtractionNode ind x y) c 
  = concat[subtractFrom x' y' | x' <- evalTree x no_constraints, y' <- evalTree y no_constraints, fits_min c (length y' - length x'), fits_min c (length y' - length x')]
evalTree (HiddenWordNode ind ys) c 
  = [x | x <- substr (concat ys), (length x) > 0, fits_constraints c x]
evalTree (ReversalNode ind ys) c 
  = map reverse (evalTree ys c)
evalTree (FirstLetterNode ind ys) c 
  = [firstLetter ys]
evalTree (LastLetterNode ind ys) c 
  = [lastLetter ys]
evalTree (PartialNode ind y) c 
  = filter (fits_constraints c) . concatMap partials $ evalTree y no_constraints
evalTree (ConsIndicatorNode x) c 
  = [""]



evalTrees :: [ParseTree] -> EvalConstraints -> [String]
evalTrees (x:[]) c 
  = evalTree x (noPrefix c)
evalTrees (x:xs) (Constraints p mx mn) 
  =
  let starts = [start | start <- evalTree x (Constraints NoPrefix mx NoMin), fits p start]
  in  concatMap f $ starts 
  where f start =  map (\x -> start ++ x) (evalTrees xs (noMin . add_partial start $ (Constraints p mx mn)))

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

no_constraints 
  = (Constraints NoPrefix NoMax NoMin)

new_constraint n 
  = Constraints (Prefix []) (Max n) (Min n)

is_prefix_with (Prefix p) x 
  = is_prefix (p ++ x)
is_prefix_with NoPrefix x 
  = True

is_lte_max (Max mx) n 
  = n <= mx 
is_lte_max NoMax n 
  = True

is_gte_min (Min mn) n 
  = n >= mn 
is_gte_min NoMin n 
  = True

is_less_than_min (Min mn) n 
  = n < mn 
is_less_than_min NoMin n 
  = False

extend_prefix (Prefix p) x 
  = Prefix (p ++ x)
extend_prefix NoPrefix x 
  = NoPrefix

fits_max (Constraints p mx mn) x 
  = is_lte_max mx x
fits_min (Constraints p mx mn) x 
  = is_gte_min mn x
fits_constraints (Constraints p mx mn) x 
  = (fits p x) && (fits mx x) && (fits mn x)

add_partial :: String -> EvalConstraints -> EvalConstraints
add_partial x (Constraints p mx mn) 
  = decreaseMax (length x) $ (Constraints (extend_prefix p x) mx mn) 

decreaseMax :: Int -> EvalConstraints -> EvalConstraints
decreaseMax n (Constraints p (Max mx) mn) 
  = Constraints p (Max (mx - n)) mn
decreaseMax n (Constraints p NoMax mn) 
  = Constraints p NoMax mn

increaseMin :: Int -> EvalConstraints -> EvalConstraints
increaseMin n (Constraints p mx (Min mn)) 
  = Constraints p mx (Min (mn + n))
increaseMin n (Constraints p mx NoMin) 
  = Constraints p mx NoMin

decreaseMin :: Int -> EvalConstraints -> EvalConstraints
decreaseMin n (Constraints p mx (Min mn)) 
  = Constraints p mx (Min (mn - n))
decreaseMin n (Constraints p mx NoMin) 
  = Constraints p mx NoMin

maxC :: EvalConstraints -> MaxLength
maxC (Constraints p mx mn) 
  = mx

minC :: EvalConstraints -> MinLength
minC (Constraints p mx mn) 
  = mn

prefC :: EvalConstraints -> PrefixConstraint
prefC (Constraints p mx mn) 
  = p

noPrefix :: EvalConstraints -> EvalConstraints
noPrefix (Constraints p mx mn) 
  = (Constraints NoPrefix mx mn)

noMin :: EvalConstraints -> EvalConstraints
noMin (Constraints p mx mn) 
  = (Constraints p mx NoMin)

noMax :: EvalConstraints -> EvalConstraints
noMax (Constraints p mx mn) 
  = (Constraints p NoMax mn)

{-fits :: a -> String -> Bool
fits (Max n) s 
  = True
fits (Prefix p) s 
  = True
-}

{- instance Ord MinLength where
	(<) (Min n) = (<) n
	(<) NoMin = True
	(>) NoMin = True
	(>) (Min n) = (>) n
	-}



evaluate :: [Parse] -> [Answer]
evaluate 
  = concatMap eval 

anagrams :: String -> [String]
anagrams [] 
  = [[]]
anagrams xs 
  = [x:ys | x<- nub xs, ys <- anagrams $ delete x xs]

insertInto :: String -> String -> [String] 
insertInto xs [] 
  = [xs]
insertInto xs (y:ys) 
  = [y:(xs ++ ys)] ++ (map ((:) y) (insertInto xs ys)) 

subtractFrom :: String -> String -> [String] 
subtractFrom xs ys 
  = let n = (find_in xs ys 0 0) in if n == -1 then [] else [remove_from ys n (length xs)]

remove_from ys 0 0 
  = ys
remove_from (y:ys) 0 m 
  = remove_from ys 0 (m-1)
remove_from (y:ys) n m 
  = y:(remove_from ys (n-1) m)

find_in [] ys n f 
  = n
find_in xs [] n f 
  = -1
find_in (x:xs) (y:ys) n 0 
  = if x==y 
              then find_in xs ys n 1 
              else find_in (x:xs) (ys) (n+1) 0
find_in (x:xs) (y:ys) n 1 
  = if x==y 
              then find_in xs ys n 1
              else -1




substr [] 
  = [[]]
substr (x:xs) 
  = (map ((:) x) (contiguoussubstr xs)) ++ substr xs 

contiguoussubstr  [] 
  = [[]]
contiguoussubstr (x:xs) 
  = [[x]] ++ (map ((:) x) (contiguoussubstr xs))

firstLetter 
  = map head

lastLetter 
  = map last

missing_center xs 
  = concat . nub . map (\x -> subtractFrom x xs) . substr . strip_toptail $ xs
strip_toptail 
  = reverse . drop 1 . reverse . drop 1

partials :: String -> [String]
partials x 
  =  top_substrings x ++ tail_substrings x ++ missing_center x

top_substrings :: String -> [String]
top_substrings [] 
  = []
top_substrings (x:[]) 
  = []  
top_substrings (x:xs) 
  = [[x]] ++ (map  (\y -> [x] ++ y) (top_substrings xs))


tail_substrings :: String -> [String]
tail_substrings 
  = (map reverse) . top_substrings . reverse

{-
check_eval :: Parse -> [Answer]
-- check_eval x = let DefNode y z n = x in Data.List.intersect (synonyms y) ((evalTree n z))
check_eval (DefNode y z n) = map (\x -> Answer x (DefNode y z n)) (Data.Set.toList (Data.Set.intersection wordlist_extended (Data.Set.fromList (evalTree n z))))
-} 
