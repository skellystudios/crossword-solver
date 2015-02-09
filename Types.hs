module Types where 

data Clue = Clue (String, Int)
	deriving (Show)

data Clue2 = Clue2 String (Int)

type Parse = (String, ParseTree, Int)

data ParseTree = JuxtapositionIndicator [String] |
                 Concatenate [ParseTree] |
                 Juxtapose ParseTree ParseTree |
                 Synonym String |
                 Anagram Anagrind [String] |
                 Insertion InsertionIndicator ParseTree ParseTree |
                 Subtraction SubtractionIndicator ParseTree ParseTree |
                 HiddenWord HWIndicator [String] |
                 Reversal ReversalIndicator ParseTree |
                 FirstLetter FLIndicator [String] |
                 LastLetter LLIndicator [String] |
                 PartOf PartOfIndicator ParseTree 
                deriving (Show, Eq, Ord)

data Answer = Answer String Parse deriving (Show, Eq, Ord)

type Anagrind = [String]
type InsertionIndicator = [String] 
type SubtractionIndicator = [String] 
type ReversalIndicator = [String] 
type HWIndicator = [String] 
type FLIndicator = [String] 
type LLIndicator = [String] 
type PartOfIndicator = [String] 

data MaxLength = Max Int | NoMax
data MinLength = Min Int | NoMin

data EvalConstraints = Constraints PrefixConstraint MaxLength MinLength

data PrefixConstraint = Prefix String | Unconstrained | NoPrefix


get_parse (Answer s p) = p
get_solution (Answer s p) = s
