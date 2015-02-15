module Evaluation where

import Debug.Trace
import Data.List
import Data.Char
import Data.Binary
import Data.List 

import Utilities
import Utils
import Types
import Dictionary
import Wordlists

eval :: (Int, Parse) -> [Answer]
eval (i, (def, tree, n))
  = trace (show i) ([Answer x (def, tree, n) | 
       x <- evalTree tree (Constraints Nothing (Just n) (Just n))])

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
                  s <- evalTree t' (resetMin (resetPrefix c)), 
                  let n = length s,
                  s' <- evalTree t (decreaseMax n (decreaseMin n (resetPrefix c)))]
    evalTree' (Subtraction ind t t')
      = concat [subtractFrom s s' | 
                  s <- evalTree t noConstraints, 
                  let n = length s,
                  s' <- evalTree t' (increaseMax n (increaseMin n c))]
    evalTree' (HiddenWord ind ws)
      = [subs | subs <- substrings (concat ws)]
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
  = evalTree t c
evalTrees (t : ts) c
  = concatMap evalRest sols
  where 
    evalRest s = trace s (map (s++) (evalTrees ts (decreaseMax n (decreaseMin n (extendPrefix s c)))))
               where
                 n = length s
    sols = [s' | s' <- evalTree t (resetMin c)]

--evaluate :: [Parse] -> [Answer]
evaluate 
  = concatMap eval 


--------- CONSTRAINTS ----------

data Constraints 
  = Constraints {prefixConstraint :: Maybe String, 
                 minConstraint :: Maybe Int, 
                 maxConstraint :: Maybe Int}

add :: Maybe Int -> Int -> Maybe Int
add (Just x) y
  = Just (max (x + y) 0)
add Nothing y
  = Nothing

append :: Maybe String -> String -> Maybe String
append (Just s) s'
  = Just (s ++ s')
append Nothing s'
  = Nothing

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
extendPrefix s (Constraints p mn mx)
  = Constraints (append p s) mn mx

increaseMax :: Int -> Constraints -> Constraints
increaseMax n (Constraints p mn mx) 
  = Constraints p mn (add mx n) 

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

