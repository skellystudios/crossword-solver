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
       x <- evalTree tree (Constraints Nothing (Just n) (Just n))]

data Constraints 
  = Constraints {prefixConstraint :: Maybe String, 
                 minConstraint :: Maybe Int, 
                 maxConstraint :: Maybe Int}

add :: Maybe Int -> Int -> Maybe Int
add (Just x) y
  = Just (x + y)
add Nothing y
  = Nothing

append :: Maybe String -> String -> Maybe String
append (Just s) s'
  = Just (s ++ s')
append Nothing s'
  = Nothing

evalTree :: ParseTree -> Constraints -> [String]
evalTree t c
  = filter (satisfies c) (evalTree' t)
  where
    evalTree' Null
      = []
    evalTree' (Ident s)
      = filter (satisfies c) [s]
    evalTree' (Anagram ind ws)
      = delete s (anagrams s)
      where
        s = concat ws
    evalTree' (Synonym x)
      = synonyms x ++ [x]
    evalTree' (Concatenate xs)
      = evalTrees xs c 
    evalTree' (Insertion ind t t')
      = concat [insertInto s' s | 
                  s <- evalTree t' (resetMin . resetPrefix $ c), 
                  s' <- evalTree t (decreaseMax (length s) . decreaseMin (length s) . resetPrefix $ c)]
    evalTree' (Subtraction ind t t')
      = concat [subtractFrom s s' | 
                  s <- evalTree t noConstraints, 
                  s' <- evalTree t' noConstraints, 
                  geqMin (length s' - length s) c]
    evalTree' (HiddenWord ind ws)
      = [subs | subs <- substrings (concat ws), 
                length subs > 0]
    evalTree' (Reversal ind t)
      = map reverse (evalTree t c)
    evalTree' (FirstLetter ind ys)
      = [map head ys]
    evalTree' (LastLetter ind ys)
      = [map last ys]
    evalTree' (PartOf ind t)
      = concatMap partials (evalTree t noConstraints)
    evalTree' (JuxtapositionIndicator ind)
      = [""]

evalTrees :: [ParseTree] -> Constraints -> [String]
evalTrees [t] c 
  = evalTree t (resetPrefix c)
evalTrees (t : ts) c
  = concatMap evalRest s
  where 
    evalRest s = map (s++) (evalTrees ts (resetMin . extendPrefix s $ c))
    s = [s' | s' <- evalTree t (resetMin . resetPrefix $ c), checkPrefix s' c]

evaluate :: [Parse] -> [Answer]
evaluate 
  = concatMap eval 


--------- CONSTRAINTS

checkPrefix s 
  = maybe True (\s' -> isPrefix (s' ++ s)) . prefixConstraint

geqMin n 
  = maybe True (n>=) . minConstraint

leqMax n 
  = maybe True (n<=) . maxConstraint

noConstraints 
  = (Constraints Nothing Nothing Nothing)

satisfies c s
  = checkPrefix s c && geqMin n c && leqMax n c
  where
    n = length s

extendPrefix :: String -> Constraints -> Constraints
extendPrefix s 
  = decreaseMax (length s) . addToPrefix s

addToPrefix :: String -> Constraints -> Constraints
addToPrefix s (Constraints p mn mx)
  = Constraints (append p s) mn mx

decreaseMax :: Int -> Constraints -> Constraints
decreaseMax n (Constraints p mn mx) 
  = Constraints p mn (add mx (-n)) 

increaseMin :: Int -> Constraints -> Constraints
increaseMin n (Constraints p mn mx) 
  = Constraints p (add mn n) mx

decreaseMin :: Int -> Constraints -> Constraints
decreaseMin n (Constraints p mn mx) 
  = Constraints p (add mn (-n)) mx

resetPrefix :: Constraints -> Constraints
resetPrefix (Constraints p mn mx)
  = Constraints Nothing mn mx

resetMin :: Constraints -> Constraints
resetMin (Constraints p mn mx)
  = Constraints p Nothing mx 

resetMax :: Constraints -> Constraints
resetMax (Constraints p mn mx)
  = Constraints p mn Nothing

------------ UTILITIES ----------

anagrams :: String -> [String]
anagrams [] 
  = [[]]
anagrams s 
  = [c : s' | c <- nub s, s' <- anagrams (s \\ [c])]

insertInto :: String -> String -> [String] 
insertInto s [] 
  = [s]
insertInto s (c : cs) 
  = [c : (s ++ cs)] ++ (map (c :) (insertInto s cs)) 

subtractFrom' :: String -> String -> [String] 
subtractFrom' s s'
  = [s1 ++ s3 | (s1, s2, s3) <- split3 s', s2 == s]

subtractFrom s s' 
  | n == -1 = [] 
  | otherwise = [removeFrom s' n (length s)]
  where
     n = (findIn s s' 0 0)

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

