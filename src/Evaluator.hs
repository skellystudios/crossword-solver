module Evaluator
  (
    evaluateConcatenatedParseTrees,
    evaluate, 
    chooseAnswer
  ) where

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

    evaluateParseTree' (SynC w) cs
      = [w] -- Not this, but works for now

    evaluateParseTree' pt@(AnagC i w) cs
      = evaluateAnagramClue pt cs-- Not this, but works for now

    evaluateParseTree' (JuxtC _ pt1 pt2) cs
      = undefined

    evaluateParseTree' (ConcatC pts) cs
      = evaluateConcatenatedParseTrees pts cs

evaluateConcatenatedParseTrees  :: [ParseTree]
                                -> Constraints
                                -> [Phrase]

evaluateConcatenatedParseTrees [pt] cs 
  = evaluateParseTree pt cs

evaluateConcatenatedParseTrees (pt : pts) cs 
  = do
      let minPtL  = minLength pt
          cs'     = withMax (subtract minPtL) (withNoMin cs)

      phr <- evaluateParseTree pt cs'
      guard (isPrefixOfWordWith cs' phr)

      return ""

evaluateAnagramClue :: ParseTree -> Constraints-> [Phrase]
evaluateAnagramClue (AnagC i ws) cs
  = anagrams . concat $ ws


isPrefixOfWordWith :: Constraints -> Phrase -> Bool
isPrefixOfWordWith cs phr
  = maybe True (\p -> isPrefixOfWord (p ++ phr)) (prefix cs)

evaluate :: [ParsedClue] -> [Answer]
evaluate = concatMap evaluateParsedClue 

evaluateParsedClue :: ParsedClue -> [Answer]
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