module Types where

type Word       = String
type Words      = [Word]
type Indicator  = Words

newtype Clue
  = Clue (Int, String)
  deriving (Eq, Show)

data ParsedClue
  = NullC
  | IdentC Word
  | JuxtC Indicator ParsedClue ParsedClue
  | ConcatC [ParsedClue]
  | SynC Word
  | AnagC Indicator Words
  | InsertC Indicator ParsedClue ParsedClue
  | SubC Indicator ParsedClue ParsedClue
  | HiddenC Indicator Words
  | RevC Indicator ParsedClue
  | FirstsC Indicator Words
  | LastsC Indicator Words
  | PartC Indicator ParsedClue
  deriving (Eq, Ord, Show)
