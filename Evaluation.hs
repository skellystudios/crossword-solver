module Evaluation where

import Debug.Trace
import Data.List
import Data.Char
import Data.Binary
import Data.List 

import Utilities
import Types
import Databases
import LengthFunctions
import ManualData
import Prefixes

--evaluate :: SynonymTable -> [Parse] -> [Answer]
evaluate synTable ts 
  = concatMap eval ts
  where
    eval (i, (def, tree, n)) 
      = trace (show i) ([Answer x (def, tree, n) | 
           x <- evalTree tree (Constraints (Just "") (Just n) (Just n))])
    evalTree :: ParseTree -> Constraints -> [String]
    evalTree t c
      = filter (satisfies c) (evalTree' t)
      where
        evalTree' Null
          = []
        evalTree' (Ident s)
          = [s]
        evalTree' (Anagram ind ws)
          = anagrams (concat ws)
        evalTree' (Synonym x)
          = synonyms x 
        evalTree' t@(Concatenate ts)
          = evalTrees ts c (minLength synTable t)  
        evalTree' (Insertion ind t t')
          = concat [insertInto s s' | 
                      s' <- evalTree t' (resetMin (resetPrefix c)), 
                      let n = length s',
                      s <- evalTree t (shiftBounds (-n) (resetPrefix c))]
        evalTree' (Subtraction ind t t')
          = concat [subtractFrom s s' | 
                      s <- evalTree t noConstraints, 
                      let n = length s,
                      s' <- evalTree t' (shiftBounds n c)]
        evalTree' (HiddenWord ind ws)
          = [subs | subs <- substrings (concat ws)]
        evalTree' (Reversal ind t)
          = map reverse (evalTree t (resetPrefix c))
        evalTree' (FirstLetter ind ys)
          = [map head ys]
        evalTree' (LastLetter ind ys)
          = [map last ys]
        evalTree' (PartOf ind t)
          = concatMap partials (evalTree t noConstraints)
        evalTree' (Juxtaposition ind t t')
          = evalTree' (Concatenate [t, t'])
    
    -- m is the number of 'reserved' places and this gets reduced by the 
    -- minimum length the current tree in each call to evalTrees (n).
    -- The max constraint is initially that of the clue as a whole, but
    -- gets reduced by the length of the current prefix (k) each time.
    evalTrees :: [ParseTree] -> Constraints -> Int -> [String]
    evalTrees [t] c m
      = evalTree t (addToMax (n - m) (resetMin c))
      where
        n = minLength synTable t
    evalTrees (t : ts) c m
      = concatMap evalRest sols
      where 
        n = minLength synTable t
        evalRest s 
          | checkPrefix s c = map (s++) (evalTrees ts c' (m - n))
          | otherwise       = []
                            where
                              k = length s
                              c' = addToMax (-k) (extendPrefix s c)
        sols = [s' | s' <- evalTree t (addToMax (n - m) (resetMin c))]
    
fromJust (Just x) = x
fromJust _ = "NULL"

--------- CONSTRAINTS ----------

data Constraints 
  = Constraints {prefixConstraint :: Maybe String, 
                 minConstraint :: Maybe Int, 
                 maxConstraint :: Maybe Int}
  deriving (Show)

add :: Int -> Maybe Int -> Maybe Int
add x
  = fmap ((max 0) . (+ x))

addString :: String -> Maybe String -> Maybe String
addString s 
  = fmap (++s)

checkPrefix :: [Char] -> Constraints -> Bool
checkPrefix s 
  = maybe True (\s' -> isPrefix (s' ++ s)) . prefixConstraint

geqMin :: Int -> Constraints -> Bool
geqMin n 
  = maybe True (n>=) . minConstraint

leqMax :: Int -> Constraints -> Bool
leqMax n 
  = maybe True (n<=) . maxConstraint

satisfies :: Constraints -> String -> Bool
satisfies c s
  = checkPrefix s c && geqMin n c && leqMax n c
  where
    n = length s

extendPrefix :: String -> Constraints -> Constraints
extendPrefix s (Constraints p mn mx)
  = Constraints (addString s p) mn mx

shiftBounds :: Int -> Constraints -> Constraints
shiftBounds n (Constraints p mn mx)
  = Constraints p (add n mn) (add n mx)

noConstraints 
  = (Constraints Nothing Nothing Nothing)

resetPrefix :: Constraints -> Constraints
resetPrefix (Constraints p mn mx)
  = Constraints Nothing mn mx

resetMin :: Constraints -> Constraints
resetMin (Constraints p mn mx)
  = Constraints p Nothing mx 

resetMax :: Constraints -> Constraints
resetMax (Constraints p mn mx)
  = Constraints p mn Nothing

addToMax :: Int -> Constraints -> Constraints
addToMax n (Constraints p mn mx)
  = Constraints p mn (add n mx)
