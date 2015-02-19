module Types where 

data Clue = Clue (String, Int)
	deriving (Show)

data Clue2 = Clue2 String (Int)

type SynonymTable = [([String], (Int, Int))]

type Parse = (String, ParseTree, Int)

data ParseTree = Null |
                 Ident String |
                 JuxtapositionIndicator [String] |
                 Concatenate [ParseTree] |
                 Synonym String |
                 Anagram Anagrind [String] |
                 Insertion InsertionIndicator ParseTree ParseTree |
                 Subtraction SubtractionIndicator ParseTree ParseTree |
                 HiddenWord HiddenWordIndicator [String] |
                 Reversal ReversalIndicator ParseTree |
                 FirstLetter FirstLetterIndicator [String] |
                 LastLetter LastLetterIndicator [String] |
                 PartOf PartOfIndicator ParseTree 
                deriving (Show, Eq, Ord)

data Answer = Answer String Parse 
            deriving (Eq, Ord, Show)

getParse (Answer s p)
  = p
getAnswer (Answer s p)
  = s

type Anagrind = [String]
type InsertionIndicator = [String] 
type SubtractionIndicator = [String] 
type ReversalIndicator = [String] 
type HiddenWordIndicator = [String] 
type FirstLetterIndicator = [String] 
type LastLetterIndicator = [String] 
type PartOfIndicator = [String] 

