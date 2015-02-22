module LengthFunctions where

import Data.Maybe
import Types
import Databases
import Indicators
import ManualData

------------ LENGTH EVALUATION FUNCTIONS -----------------

minLength :: ParseTree -> [([String], (Int, Int))] -> Int
minLength Null table
  = 0
minLength (Ident s) table
  = length s
minLength (Concatenate trees) table
  = (sum . map (flip minLength table)) trees
minLength (Anagram ind strings) table
  = (length . concat) strings
minLength (HiddenWord ind strings) table
  = 2
minLength (Insertion ind tree1 tree2) table
  = (minLength tree1 table) + (minLength tree2 table)
minLength (Subtraction ind tree1 tree2) table
  = max (minLength tree1 table - maxLength tree2 table) 1
minLength (Reversal ind tree) table
  = minLength tree table
minLength (Synonym string) table
  = fst (maybe (error string) id (lookup (words string) table)) -- minimum (map length (synonyms string))
minLength (FirstLetter ind strings) table
  = length strings
minLength (LastLetter ind strings) table
  = length strings
minLength (PartOf ind tree) table
  = 1
minLength (Juxtaposition ind tree1 tree2) table
  = minLength (Concatenate [tree1, tree2]) table

maxLength Null table
  = 0
maxLength (Ident s) table
  = length s
maxLength (Concatenate trees) table
  = (sum . map (flip maxLength table)) trees
maxLength (Anagram ind strings) table
  = (length . concat) strings
maxLength (HiddenWord ind strings) table
  = (length . concat $ strings) - 2
maxLength (Insertion ind tree1 tree2) table
  = (maxLength tree1 table) + (maxLength tree2 table)
maxLength (Subtraction ind tree1 tree2) table
  = max (maxLength tree1 table - minLength tree2 table) 1
maxLength (Reversal ind tree) table
  = maxLength tree table
maxLength (Synonym string) table
  = snd (maybe (error string) id (lookup (words string) table)) -- maximum (map length (synonyms string))
maxLength (FirstLetter ind strings) table
  = length strings
maxLength (LastLetter ind strings) table
  = length strings
maxLength (PartOf ind tree) table
  = (maxLength tree table) - 1
maxLength (Juxtaposition ind tree1 tree2) table
  = maxLength (Concatenate [tree1, tree2]) table
