module Types where

type Phrase       = String
type Length       = Int

type Word         = String
type Words        = [Word]

type Definition   = Phrase
type Indicator    = Words

type PairsOf a    = [(a, a)]
type TriplesOf a  = [(a, a, a)]

newtype Clue
  = Clue (Phrase, Length)
  deriving (Eq, Show)

newtype ParsedClue
  = ParsedClue (Clue, Definition, Indicator, ParseTree)
  deriving (Eq, Show)

data ParseTree
  = NullC
  | IdentC Word
  | JuxtC Indicator ParseTree ParseTree
  | ConcatC [ParseTree]
  | SynC Phrase
  | AnagC Indicator Words
  | InsertC Indicator ParseTree ParseTree
  | SubC Indicator ParseTree ParseTree
  | HiddenC Indicator Words
  | RevC Indicator ParseTree
  | FirstsC Indicator Words
  | LastsC Indicator Words
  | PartC Indicator ParseTree
  deriving (Eq, Show)

newtype Answer
  = Answer (Phrase, ParsedClue)
  deriving (Eq, Show)
