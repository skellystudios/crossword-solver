module Learn where

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

type ParseTree' = ((String, Int), ParseTree'')

data ParseTree'' = Null' |
                   Ident' String |
                   Juxtaposition' JuxtapositionIndicator [ParseTree'] [ParseTree'] |
                   Concatenate' [[ParseTree']] |
                   Synonym' String |
                   Anagram' Anagrind [String] |
                   Insertion' InsertionIndicator [ParseTree'] [ParseTree'] |
                   Subtraction' SubtractionIndicator [ParseTree'] [ParseTree'] |
                   HiddenWord' HiddenWordIndicator [String] |
                   Reversal' ReversalIndicator [ParseTree'] |
                   FirstLetter' FirstLetterIndicator [String] |
                   LastLetter' LastLetterIndicator [String] |
                   PartOf' PartOfIndicator [ParseTree'] 
                 deriving (Show, Eq, Ord)

getScore = snd . fst

maxScore 
  = maximum . map getScore

score
  = sum . map maxScore

--matchP :: String -> Parse -> [ParseTree']
matchP s (_, (def, t, n))
  = match s t

match :: String -> ParseTree -> [ParseTree']
match s Null
  = []
match s (Ident s')
  | s == s' = [((s, length s), Ident' s')]
  | otherwise = []
match s (Anagram ind ws)
  | sameLetters s (concat ws) = [((s, length s), Anagram' ind ws)]
  | otherwise = []
match s (Synonym x)
  = [((s, n), Synonym' x)]
  where
    n = if elem s (synonyms x) then length s else 0
match s (Concatenate ts)
  = [((s, score ts'), Concatenate' ts') |
       p <- partitions s, length p == length ts,
       let ts' = zipWith match p ts,
       all (not.null) ts']
match s (Insertion ind t t')
  = [((s, score [m, m']), Insertion' ind m m') |
       (s1, s2, s3) <- split3 s,
       let m = match s2 t,
       let m' = match (s1 ++ s3) t',
       not (null m) && not (null m')]
match s (HiddenWord ind ws)
  | elem s (substrings (concat ws)) = [((s, length s), HiddenWord' ind ws)]
  | otherwise = []
match s (Reversal ind t)
  = [((s, maxScore m), Reversal' ind m)]
  where
    m =  match (reverse s) t
match s (FirstLetter ind ys)
  | s == map head ys = [((s, length s), FirstLetter' ind ys)]
  | otherwise = []
match s (LastLetter ind ys)
  | s == map last ys = [((s, length s), LastLetter' ind ys)]
  | otherwise = []
match s (PartOf ind t)
  = [((s, maxScore m), PartOf' ind m) |
       s' <- partials s,
       let m = match s' t,
       not (null m)]
match s (Juxtaposition ind t t')
  = [((s, score ts'), Juxtaposition' ind (ts' !! 0) (ts' !! 1)) |
       p <- partitions s, length p == length ts,
       let ts' = zipWith match p ts,
       all (not.null) ts'] 
  where
    ts = [t, t']

