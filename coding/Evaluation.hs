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
eval (DefNode y z n) = let constraints = (n, n) in [Answer x (DefNode y z n) | x <- evalTree z (Constraints (Prefix []) (Max n) (Min n))] 

evalTree :: ParseTree  -> EvalConstraints -> [String]
evalTree (AnagramNode x y) c = 
								let y' = (concat y) in 	
								if (isLessThanMin (minC c) (length y')) then [] else  filter (isPrefixWith (prefC c)) . (delete y') . anagrams  $ y'

evalTree (SynonymNode x) c = filter (fitsConstraints c) (syn x ++ [x])
evalTree (ConcatNode xs) c = evalTrees xs c --map concat (sequence (map (evalTree n) xs))
-- evalTree (ConsNode x y) c = [x' ++ y' | x' <- evalTree x (noPrefix c), y' <- evalTree y (Constraints NoPrefix (mx - length x') (mn - length x'))]
evalTree (InsertionNode ind x y) c = concat[insertInto x' y' | y' <- evalTree y (noMin . noPrefix $ c), x' <- evalTree x (decreaseMax (length y') . decreaseMin (length y') . noPrefix $ c)]
evalTree (SubtractionNode ind x y) c = concat[subtractFrom x' y' | x' <- evalTree x noConstraints, y' <- evalTree y noConstraints, fitsMin c (length y' - length x'), fitsMin c (length y' - length x')]
evalTree (HiddenWordNode ind ys) c = [x | x <- substr (concat ys), (length x) > 0, fitsConstraints c x]
evalTree (ReversalNode ind ys) c = map reverse (evalTree ys c)
evalTree (FirstLetterNode ind ys) c = [firstLetter ys]
evalTree (LastLetterNode ind ys) c = [lastLetter ys]
evalTree (PartialNode ind y) c = filter (fitsConstraints c) . concatMap partials $ evalTree y noConstraints
evalTree (ConsIndicatorNode x) c = [""]



evalTrees :: [ParseTree] -> EvalConstraints -> [String]
evalTrees (x:[]) c = evalTree x (noPrefix c)
evalTrees (x:xs) (Constraints p mx mn) =
  let starts = [start | start <- evalTree x (Constraints NoPrefix mx NoMin), fits p start]
  in  concatMap f $ starts 
  where f start =  map (\x -> start ++ x) (evalTrees xs (noMin . addPartial start $ (Constraints p mx mn)))

--noConstraints
--------- CONSTRAINTS

class Constraint c where
    fits :: c -> String -> Bool
instance Constraint MaxLength where
    fits mx s = isLteMax mx (length s)
instance Constraint MinLength where
    fits mn s = isGteMax mn (length s)
instance Constraint PrefixConstraint where
	fits p s = isPrefixWith p s

noConstraints = (Constraints NoPrefix NoMax NoMin)

newConstraint n = Constraints (Prefix []) (Max n) (Min n)

isPrefixWith (Prefix p) x = isPrefix (p ++ x)
isPrefixWith NoPrefix x = True

isLteMax (Max mx) n = n <= mx 
isLteMax NoMax n = True

isGteMax (Min mn) n = n >= mn 
isGteMax NoMin n = True

isLessThanMin (Min mn) n = n < mn 
isLessThanMin NoMin n = False

extendPrefix (Prefix p) x = Prefix (p ++ x)
extendPrefix NoPrefix x = NoPrefix

fitsMax (Constraints p mx mn) x = isLteMax mx x
fitsMin (Constraints p mx mn) x = isGteMax mn x
fitsConstraints (Constraints p mx mn) x = (fits p x) && (fits mx x) && (fits mn x)

addPartial :: String -> EvalConstraints -> EvalConstraints
addPartial x (Constraints p mx mn) = decreaseMax (length x) $ (Constraints (extendPrefix p x) mx mn) 

decreaseMax :: Int -> EvalConstraints -> EvalConstraints
decreaseMax n (Constraints p (Max mx) mn) = Constraints p (Max (mx - n)) mn
decreaseMax n (Constraints p NoMax mn) = Constraints p NoMax mn

increaseMin :: Int -> EvalConstraints -> EvalConstraints
increaseMin n (Constraints p mx (Min mn)) = Constraints p mx (Min (mn + n))
increaseMin n (Constraints p mx NoMin) = Constraints p mx NoMin

decreaseMin :: Int -> EvalConstraints -> EvalConstraints
decreaseMin n (Constraints p mx (Min mn)) = Constraints p mx (Min (mn - n))
decreaseMin n (Constraints p mx NoMin) = Constraints p mx NoMin

maxC :: EvalConstraints -> MaxLength
maxC (Constraints p mx mn) = mx

minC :: EvalConstraints -> MinLength
minC (Constraints p mx mn) = mn

prefC :: EvalConstraints -> PrefixConstraint
prefC (Constraints p mx mn) = p

noPrefix :: EvalConstraints -> EvalConstraints
noPrefix (Constraints p mx mn) = (Constraints NoPrefix mx mn)

noMin :: EvalConstraints -> EvalConstraints
noMin (Constraints p mx mn) = (Constraints p mx NoMin)

noMax :: EvalConstraints -> EvalConstraints
noMax (Constraints p mx mn) = (Constraints p NoMax mn)

evaluate :: [Parse] -> [Answer]
evaluate = concatMap eval 

anagrams :: String -> [String]
anagrams [] = [[]]
anagrams xs = [x:ys | x<- nub xs, ys <- anagrams $ delete x xs]

insertInto :: String -> String -> [String] 
insertInto xs [] = [xs]
insertInto xs (y:ys) = [y:(xs ++ ys)] ++ (map ((:) y) (insertInto xs ys)) 

subtractFrom :: String -> String -> [String] 
subtractFrom xs ys = let n = (findIn xs ys 0 0) in if n == -1 then [] else [removeFrom ys n (length xs)]

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


substr [] = [[]]
substr (x:xs) = (map ((:) x) (contiguoussubstr xs)) ++ substr xs 

contiguoussubstr  [] = [[]]
contiguoussubstr (x:xs) = [[x]] ++ (map ((:) x) (contiguoussubstr xs))

firstLetter = map head

lastLetter = map last

missingCenter xs = concat . nub . map (\x -> subtractFrom x xs) . substr . stripTopTail $ xs
stripTopTail = reverse . drop 1 . reverse . drop 1

partials :: String -> [String]
partials x =  topSubstrings x ++ tailSubstrings x ++ missingCenter x

topSubstrings :: String -> [String]
topSubstrings [] = []
topSubstrings (x:[]) = []  
topSubstrings (x:xs) = [[x]] ++ (map  (\y -> [x] ++ y) (topSubstrings xs))

tailSubstrings :: String -> [String]
tailSubstrings = (map reverse) . topSubstrings . reverse
