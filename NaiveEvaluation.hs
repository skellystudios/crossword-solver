module NaiveEvaluation where

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
eval (def, parseTree, n) 
  = [Answer sol (def, parseTree, n) | sol <- evalTree parseTree]  

evalTree :: ParseTree -> [String]
evalTree (Anagram ind ws) 
  = anagrams (concat ws)
evalTree (Synonym ws)  
  = synonyms ws ++ [ws]
evalTree (Juxtapose ws ws')  
  = [sol ++ sol' | sol <- evalTree ws, sol' <- evalTree ws']
evalTree (Insertion ind ws ws')  
  = concat [insertInto sol sol' | sol' <- evalTree ws', sol <- evalTree ws]
evalTree (Subtraction ind ws ws')  
  = concat[subtractFrom sol sol' | sol <- evalTree ws, sol' <- evalTree ws']
evalTree (HiddenWord ind ws)  
  = [s | s <- substr (concat ws)]
evalTree (Reversal ind ws)  
  = map reverse (evalTree ws)
evalTree (FirstLetter ind ws)  
  = [firstLetter ws]
evalTree (LastLetter ind ws)  
  = [lastLetter ws]
evalTree (PartOf ind ws)  
  = concatMap partials (evalTree ws)
evalTree (JuxtapositionIndicator ws)  
  = [""]

evaluate :: [Parse] -> [Answer]
evaluate 
  = concatMap eval 

anagrams :: String -> [String]
anagrams w
  = delete w (anagrams' w)
  where
    anagrams' [] 
      = [[]]
    anagrams' xs 
      = [x:ys | x <- nub xs, ys <- anagrams' $ delete x xs]

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

