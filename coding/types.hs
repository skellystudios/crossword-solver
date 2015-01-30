module Types where 

-- MS: So I'd completely forgotten about type synonyms until my writeup, so they don't feature
-- MS: Also, I use a list of strings rather than a single string for the wordplay: its a tradeoff between having to constantly unpack the list vs removing the spaces 
data Split = Def String [String] Int
	deriving (Show)
	
-- MS: The clue format isn't exactly as specified, but as all my sample clues are already formated like this, I'm loath to change. 
data Clue = Clue (String, Int)
	deriving (Show)

data Parse = DefNode String ParseTree Int
  deriving (Show, Eq, Ord)

data ParseTree = ConsIndicatorNode [String] | ConcatNode [ParseTree] | ConsNode ParseTree ParseTree | SynonymNode String | AnagramNode Anagrind [String] | InsertionNode InsertionIndicator ParseTree ParseTree | SubtractionNode SubtractionIndicator ParseTree ParseTree | HiddenWordNode HWIndicator [String] | ReversalNode ReversalIndicator ParseTree | FirstLetterNode FLIndicator [String] | LastLetterNode LLIndicator [String] | PartialNode PartialIndicator ParseTree
   deriving (Show, Eq, Ord)

data Answer = Answer String Parse deriving (Show, Eq, Ord)

data Anagrind = AIndicator [String] deriving (Show, Eq, Ord)
data InsertionIndicator = IIndicator [String] deriving (Show, Eq, Ord)
data SubtractionIndicator = SIndicator [String] deriving (Show, Eq, Ord)
data ReversalIndicator = RIndicator [String] deriving (Show, Eq, Ord)
data HWIndicator = HWIndicator [String] deriving (Show, Eq, Ord)
data FLIndicator = FLIndicator [String] deriving (Show, Eq, Ord)
data LLIndicator = LLIndicator [String] deriving (Show, Eq, Ord)
data PartialIndicator = PartialIndicator [String] deriving (Show, Eq, Ord)

data MaxLength = Max Int | NoMax
data MinLength = Min Int | NoMin

data EvalConstraints = Constraints PrefixConstraint MaxLength MinLength

data PrefixConstraint = Prefix String | Unconstrained | NoPrefix


getParse (Answer s p) = p
getSolution (Answer s p) = s