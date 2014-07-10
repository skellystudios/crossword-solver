module Evaluation where

import Data.Char
import Data.Binary
import Data.List 

import Utils
import Types
import Dictionary
import Wordlists

-- Now we evaluate
eval :: Parse -> [Answer]
eval (DefNode y z n) = let constraints = (n, n) in [Answer x (DefNode y z n) | x <- eval_tree n z] 

eval_tree :: Int -> ClueTree  -> [String]
eval_tree n (AnagramNode x y) = if length(concat(y)) > n then [] else anagrams(concat(y))
eval_tree n (Leaf x) = filter (\x -> length x <= n) (syn x ++ [x])
eval_tree n (ConsListNode xs) = eval_trees n xs --map concat (sequence (map (eval_tree n) xs))
eval_tree n (ConsNode x y) = [x' ++ y' | x' <- eval_tree n x, y' <- eval_tree (n - length x') y]
eval_tree n (InsertionNode ind x y) = concat[insertInto x' y' | x' <- eval_tree n x, y' <- eval_tree (n - (length x')) y]
eval_tree n (SubtractionNode ind x y) = concat[subtractFrom x' y' | x' <- eval_tree n x, y' <- eval_tree n y]
eval_tree n (HiddenWordNode ind ys) = [x | x <- substr (concat ys), (length x) > 0, (length x) <= n]
eval_tree n (ReversalNode ind ys) = map reverse (eval_tree n ys)
eval_tree n (FirstLetterNode ind ys) = [firstLetter ys]
eval_tree n (LastLetterNode ind ys) = [lastLetter ys]
eval_tree n (PartialNode ind y) = concat [top_tail_substrings y | y <- eval_tree n y]
eval_tree n (ConsIndicatorLeaf x) = [""]

eval_trees :: Int -> [ClueTree] -> [String]
eval_trees n (c:[]) = eval_tree n c
eval_trees n (c:clues_left) =
  let starts = eval_tree n c
  in concatMap f starts 
  where f start =  map (\x -> start ++ x) (eval_trees (n - (length start)) clues_left)


evaluate :: [Parse] -> [Answer]
evaluate = concat . (map eval) 



anagrams :: String -> [String]
anagrams [] = [[]]
anagrams xs = [x:ys | x<-xs, ys <- anagrams(delete x xs)]


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

lastLetter = concat . (map tail)

top_tail_substrings :: String -> [String]
top_tail_substrings x =  top_substrings x ++ tail_substrings x

top_substrings :: String -> [String]
top_substrings (x:[]) = []  
top_substrings (x:xs) = [[x]] ++ (map  (\y -> [x] ++ y) (top_substrings xs))


tail_substrings :: String -> [String]
tail_substrings = (map reverse) . top_substrings . reverse