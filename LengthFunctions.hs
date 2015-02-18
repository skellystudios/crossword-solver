module LengthFunctions where

import Data.Maybe
import Types
import Databases
import Indicators

------------ LENGTH EVALUATION FUNCTIONS -----------------

minLength table Null 
  = 0
minLength table (Ident s) 
  = length s
minLength table (Concatenate trees) 
  = (sum . map (minLength table)) trees
minLength table (Anagram ind strings) 
  = (length . concat) strings
minLength table (HiddenWord ind strings) 
  = 2
minLength table (Insertion ind tree1 tree2) 
  = (minLength table tree1) + (minLength table tree2)
minLength table (Subtraction ind tree1 tree2) 
  = min (minLength table tree2 - maxLength table tree1) 3
minLength table (Reversal ind tree) 
  = minLength table tree
minLength table (Synonym string) 
  = fst (maybe (error string) id (lookup (words string)  table)) -- minimum (map length (synonyms string))
minLength table (FirstLetter ind strings) 
  = length strings
minLength table (LastLetter ind strings) 
  = length strings
minLength table (Juxtapose one two) 
  = minLength table one + minLength table two
minLength table (PartOf ind tree) 
  = 1
minLength table (JuxtapositionIndicator xs) 
  = 0

maxLength table Null 
  = 0
maxLength table (Ident s) 
  = length s
maxLength table (Concatenate trees) 
  = (sum . map (maxLength table)) trees
maxLength table (Anagram ind strings) 
  = (length . concat) strings
maxLength table (HiddenWord ind strings) 
  = (length . concat $ strings) - 2
maxLength table (Insertion ind tree1 tree2) 
  = (maxLength table tree1) + (maxLength table tree2)
maxLength table (Subtraction ind tree1 tree2) 
  = max (maxLength table tree2 - minLength table tree1) 3
maxLength table (Reversal ind tree) 
  = maxLength table tree
maxLength table (Synonym string) 
  = snd (maybe (error string) id (lookup (words string) table)) -- maximum (map length (synonyms string))
maxLength table (FirstLetter ind strings) 
  = length strings
maxLength table (LastLetter ind strings) 
  = length strings
maxLength table (Juxtapose one two) 
  = maxLength table one + maxLength table two
maxLength table (PartOf ind tree) 
  = (maxLength table tree) - 1
maxLength table (JuxtapositionIndicator xs) 
  = 0
