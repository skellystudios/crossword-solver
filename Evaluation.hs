module Evaluation where

import Data.List
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
eval (def, tree, n) 
  = [Answer x (def, tree, n) | 
       x <- evalTree tree (Constraints (Prefix []) (Max n) (Min n))] 

evalTree :: ParseTree  -> EvalConstraints -> [String]
evalTree Null c
  = []
evalTree (Ident s) c
  = filter (fits_constraints c) [s]
evalTree (Anagram ind ws) c 
  = let s = concat ws 
    in 	if (is_less_than_min (minC c) (length s)) 
        then [] 
        else  filter (is_prefix_with (prefC c)) . (delete s) . anagrams  $ s
evalTree (Synonym x) c 
  = filter (fits_constraints c) (synonyms x ++ [x])
evalTree (Concatenate xs) c 
  = evalTrees xs c 
evalTree (Insertion ind x y) c 
  = concat[insertInto x' y' | y' <- evalTree y (noMin . noPrefix $ c), x' <- evalTree x (decreaseMax (length y') . decreaseMin (length y') . noPrefix $ c)]
evalTree (Subtraction ind x y) c 
  = concat[subtractFrom x' y' | x' <- evalTree x no_constraints, y' <- evalTree y no_constraints, fits_min c (length y' - length x'), fits_min c (length y' - length x')]
evalTree (HiddenWord ind ys) c 
  = [x | x <- substrings (concat ys), (length x) > 0, fits_constraints c x]
evalTree (Reversal ind ys) c 
  = map reverse (evalTree ys c)
evalTree (FirstLetter ind ys) c 
  = [firstLetter ys]
evalTree (LastLetter ind ys) c 
  = [lastLetter ys]
evalTree (PartOf ind y) c 
  = filter (fits_constraints c) . concatMap partials $ evalTree y no_constraints
evalTree (JuxtapositionIndicator x) c 
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
  = let n = (findIn xs ys 0 0) in if n == -1 then [] else [removeFrom ys n (length xs)]

removeFrom ys 0 0 
  = ys
removeFrom (y:ys) 0 m 
  = removeFrom ys 0 (m-1)
removeFrom (y:ys) n m 
  = y:(removeFrom ys (n-1) m)

findIn [] ys n f 
  = n
findIn xs [] n f 
  = -1
findIn (x:xs) (y:ys) n 0 
  = if x==y 
              then findIn xs ys n 1 
              else findIn (x:xs) (ys) (n+1) 0
findIn (x:xs) (y:ys) n 1 
  = if x==y 
              then findIn xs ys n 1
              else -1

firstLetter 
  = map head

lastLetter 
  = map last

substr [] 
  = [[]]
substr (x:xs) 
  = (map ((:) x) (contiguoussubstr xs)) ++ substr xs 

contiguoussubstr  [] 
  = [[]]
contiguoussubstr (x:xs) 
  = [[x]] ++ (map ((:) x) (contiguoussubstr xs))

substrings []     
  = []
substrings (x:xs) 
  = substrings' (x:xs) ++ substrings xs 
  where
    substrings' []     = []
    substrings' (y:ys) = [y] : [y : s | s <- substrings' ys]

partials s
  = nub (substrings s ++ map (s\\) subs)
  where
    subs = substrings s
{-
substrings []
  = [[]]
substrings (c : cs)
  = [c : cs' | cs' <- subs] ++ subs
  where
    subs = substrings cs

missing_center xs 
  = concat . nub . map (\x -> subtractFrom x xs) . substr . strip_toptail $ xs
strip_toptail 
  = reverse . drop 1 . reverse . drop 1

  = []
parts s
  = prefixes s'' ++ suffixes (tail s) ++ parts s' ++ parts s'' ++ parts (tail s)
  where
    s' = reverse . drop 1 . reverse . drop 1 $ s
    s'' = take (length s - 1) s

partials :: String -> [String]
partials x 
  =  prefixes x ++ suffixes x ++ missing_center x

prefixes :: String -> [String]
prefixes s
  = map (flip take s) [1..length s]

suffixes :: String -> [String]
suffixes s
  = take (length s) (iterate tail s)
--map reverse . prefixes . reverse
-}

{-
check_eval :: Parse -> [Answer]
-- check_eval x = let (y, z, n) = x in Data.List.intersect (synonyms y) ((evalTree n z))
check_eval (y, z, n) = map (\x -> Answer x (y, z, n)) (Data.Set.toList (Data.Set.intersection wordlist_extended (Data.Set.fromList (evalTree n z))))
-} 
