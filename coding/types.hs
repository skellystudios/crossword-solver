module Types where 

data Clue = Clue (String, Int)
	deriving (Show)

data Parse = DefNode String ClueTree Int
  deriving (Show, Eq, Ord)

data ClueTree = ConsIndicatorLeaf [String] | ConsListNode [ClueTree] | ConsNode ClueTree ClueTree | Leaf String | AnagramNode Anagrind [String] | InsertionNode InsertionIndicator ClueTree ClueTree | SubtractionNode SubtractionIndicator ClueTree ClueTree | HiddenWordNode HWIndicator [String] | ReversalNode ReversalIndicator ClueTree | FirstLetterNode FLIndicator [String] | LastLetterNode LLIndicator [String] | PartialNode PartialIndicator ClueTree
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

type MaxLength = Int
type MinLength = Int

data EvalConstraints = PrefixConstraint MaxLength MinLength

data PrefixConstraint = Prefix String | Nothing


get_parse (Answer s p) = p
get_solution (Answer s p) = s