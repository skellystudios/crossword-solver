module LengthFunctions where

import Types
import Wordlists
import Dictionary
import Utils
import Anagram

------------ LENGTH EVALUATION FUNCTIONS -----------------

minLength Null 
  = 0
minLength (Ident s) 
  = length s
minLength (Concatenate trees) 
  = (sum . map minLength) trees
minLength (Anagram ind strings) 
  = (length . concat) strings
minLength (HiddenWord ind strings) 
  = 2
minLength (Insertion ind tree1 tree2) 
  = (minLength tree1) + (minLength tree2)
minLength (Subtraction ind tree1 tree2) 
  = min (minLength tree2 - maxLength tree1) 3
minLength (Reversal ind tree) 
  = minLength tree
minLength (Synonym string) 
  = minimum (map length (synonyms string))
minLength (FirstLetter ind strings) 
  = length strings
minLength (LastLetter ind strings) 
  = length strings
minLength (Juxtapose one two) 
  = minLength one + minLength two
minLength (PartOf ind tree) 
  = 1
minLength (JuxtapositionIndicator xs) 
  = 0

maxLength Null 
  = 0
maxLength (Ident s) 
  = length s
maxLength (Concatenate trees) 
  = (sum . map maxLength) trees
maxLength (Anagram ind strings) 
  = (length . concat) strings
maxLength (HiddenWord ind strings) 
  = (length . concat $ strings) - 2
maxLength (Insertion ind tree1 tree2) 
  = (maxLength tree1) + (maxLength tree2)
maxLength (Subtraction ind tree1 tree2) 
  = max (maxLength tree2 - minLength tree1) 3
maxLength (Reversal ind tree) 
  = maxLength tree
maxLength (Synonym string) 
  = maximum (map length (synonyms string))
maxLength (FirstLetter ind strings) 
  = length strings
maxLength (LastLetter ind strings) 
  = length strings
maxLength (Juxtapose one two) 
  = maxLength one + maxLength two
maxLength (PartOf ind tree) 
  = (maxLength tree) - 1
maxLength (JuxtapositionIndicator xs) 
  = 0
