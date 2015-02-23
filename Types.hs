module Types where 

type Words = [String]

data Clue = Clue (String, Int)
	deriving (Show)

type SynonymTable = [(Words, (Int, Int))]

type Parse = (String, String, ParseTree, Int)

data ParseTree = Null |
                 Ident String |
                 Juxtaposition JuxtapositionIndicator ParseTree ParseTree |
                 Concatenate [ParseTree] |
                 Synonym String |
                 Anagram Anagrind Words |
                 Insertion InsertionIndicator ParseTree ParseTree |
                 Subtraction SubtractionIndicator ParseTree ParseTree |
                 HiddenWord HiddenWordIndicator Words |
                 Reversal ReversalIndicator ParseTree |
                 FirstLetter FirstLetterIndicator Words |
                 LastLetter LastLetterIndicator Words |
                 PartOf PartOfIndicator ParseTree 
                deriving (Show, Eq, Ord)

data Answer = Answer String Parse 
            deriving (Eq, Ord, Show)

getParse (Answer s p)
  = p
getAnswer (Answer s p)
  = s

type Anagrind = Words
type InsertionIndicator = Words 
type SubtractionIndicator = Words 
type ReversalIndicator = Words 
type HiddenWordIndicator = Words 
type FirstLetterIndicator = Words 
type LastLetterIndicator = Words 
type PartOfIndicator = Words 
type JuxtapositionIndicator = Words

type Pairs = [(Words, Words)]
type Triples = [(Words, Words, Words)]
