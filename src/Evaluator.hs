module Evaluator
  where

import Control.Monad
import Data.Set (Set,fromList,member) 
import Data.List 

import Constraints
import Types
import Wordlists
import Synonyms


{-
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
 -}

evaluateParseTree :: ParseTree -> Constraints -> [Phrase]
evaluateParseTree pt cs
  = evaluateParseTree' pt cs
  where
    evaluateParseTree' NullC cs
      = []

    evaluateParseTree' (IdentC w) cs
      = [w]

    evaluateParseTree' pt@(SynC w) cs
      = evaluateSynClue pt cs 

    evaluateParseTree' pt@(AnagC i w) cs
      = evaluateAnagramClue pt cs

    evaluateParseTree' (JuxtC _ pt1 pt2) cs
      = undefined

    evaluateParseTree' (ConcatC pts) cs
      = evaluateConcatenatedParseTrees pts cs

evaluateConcatenatedParseTrees  :: [ParseTree] -> Constraints -> [Phrase]
evaluateConcatenatedParseTrees [pt] cs 
  = evaluateParseTree pt cs

evaluateConcatenatedParseTrees (pt : pts) cs 
  = do
      let minPtL  = minLength pt
          cs'     = withMax (subtract minPtL) (withNoMin cs)

      phr <- evaluateParseTree pt cs'
      guard (isPrefixOfWordWith cs' phr)    
      guard (phr /= "")
      map ((++) phr) (evaluateConcatenatedParseTrees pts cs)

evaluateAnagramClue :: ParseTree -> Constraints-> [Phrase]
evaluateAnagramClue (AnagC i ws) cs
  = anagrams . concat $ ws

evaluateSynClue :: ParseTree -> Constraints-> [Phrase]
evaluateSynClue (SynC w) cs
  = synonyms w -- Needs to check constraints


isPrefixOfWordWith :: Constraints -> Phrase -> Bool
isPrefixOfWordWith cs phr
  = maybe True (\p -> isPrefixOfWord (p ++ phr)) (prefix cs)

evaluate :: [ParsedClue] -> [Answer]
evaluate = concatMap evaluateParsedClue 

evaluateParsedClue :: ParsedClue -> [Answer]
evaluateParsedClue 
  pc@(ParsedClue ((Clue (_, len)), def, indicator, (SynC c)))
  = [] 

evaluateParsedClue pc@(ParsedClue ((Clue (_, len)), def, indicator, pt)) 
  = do
    let constraints = makeConstraints Nothing  Nothing Nothing --(Just "", Just len, Just len)
    phrs <- evaluateParseTree pt constraints
    guard (isInWordlist phrs)
    return $ Answer (phrs, pc)

chooseAnswer :: [Answer] -> [Answer]
chooseAnswer = filter isCorrectAnswer 

isCorrectAnswer :: Answer -> Bool
isCorrectAnswer (Answer (ans, ParsedClue (c, d, i, p))) 
  = Data.Set.member 
      ans 
      (Data.Set.fromList (synonyms d))


{----- Helper methods to help evaluate ----}

anagrams :: String -> [String]
anagrams [] = [[]]
anagrams xs = [x:ys | x<- nub xs, ys <- anagrams $ delete x xs]