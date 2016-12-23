module Types where

import Prelude hiding (Word)

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
  | IdentC Word -- Done
  | JuxtC Indicator ParseTree ParseTree -- Done
  | ConcatC [ParseTree] -- Done
  | SynC Phrase -- Done
  | AnagC Indicator Words -- Done
  | InsertC Indicator ParseTree ParseTree -- Done
  | SubC Indicator ParseTree ParseTree
  | HiddenC Indicator Words -- Done
  | RevC Indicator ParseTree
  | FirstsC Indicator Words
  | LastsC Indicator Words
  | PartC Indicator ParseTree
  deriving (Eq, Show)

newtype Answer
  = Answer (Phrase, ParsedClue)
  deriving (Eq, Show)
