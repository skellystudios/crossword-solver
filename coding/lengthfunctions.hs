module LengthFunctions where

import Types
import Wordlists
import Dictionary
import Utils
import Anagram

------------ LENGTH EVALUATION FUNCTIONS -----------------

minLength (ConsListNode trees) = (sum . map minLength) trees
minLength (AnagramNode ind strings) = (length . concat) strings
minLength (HiddenWordNode ind strings) = 2
minLength (InsertionNode ind tree1 tree2) = (minLength tree1) + (minLength tree2)
minLength (SubtractionNode ind tree1 tree2) = minimum[(minLength tree2) - (maxLength tree1),3]
minLength (ReversalNode ind tree) = minLength tree
minLength (SynonymNode string) = let x = minimum ( map length (string : syn string)) in x
minLength (FirstLetterNode ind strings) = length strings
minLength (LastLetterNode ind strings) = length strings
minLength (ConsNode one two) = minLength one + minLength two
minLength (PartialNode ind tree) = 1
minLength (ConsIndicatorNode xs) = 0

maxLength (ConsListNode trees) = (sum . map maxLength) trees
maxLength (AnagramNode ind strings) = (length . concat) strings
maxLength (HiddenWordNode ind strings) = (length . concat $ strings) - 2
maxLength (InsertionNode ind tree1 tree2) = (maxLength tree1) + (maxLength tree2)
maxLength (SubtractionNode ind tree1 tree2) = maximum[(maxLength tree2) - (minLength tree1),3]
maxLength (ReversalNode ind tree) = maxLength tree
maxLength (SynonymNode string) = let x = maximum ( map length (string : syn string)) in x
maxLength (FirstLetterNode ind strings) = length strings
maxLength (LastLetterNode ind strings) = length strings
maxLength (ConsNode one two) = maxLength one + maxLength two
maxLength (PartialNode ind tree) = (maxLength tree) - 1
maxLength (ConsIndicatorNode xs) = 0