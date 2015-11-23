module Evaluator
  (
    evaluateConcatenatedParseTrees,
    evaluate
  ) where

import Control.Monad

import Constraints
import Types
import Wordlists


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
evaluateParseTree
  = undefined
  where
    evaluateParseTree' NullC
      = []

    evaluateParseTree' (IdentC w)
      = [w]

    evaluateParseTree' (JuxtC _ pt1 pt2)
      = undefined

    evaluateParseTree' (ConcatC pts)
      = undefined

evaluateConcatenatedParseTrees  :: [ParseTree]
                                -> Constraints
                                -> Length
                                -> [Phrase]

evaluateConcatenatedParseTrees [pt] cs maxL
  = undefined

evaluateConcatenatedParseTrees (pt : pts) cs maxL
  = do
      let minPtL  = minLength pt
          cs'     = withMax (subtract minPtL) (withNoMin cs)

      phr <- evaluateParseTree pt cs'
      guard (isPrefixOfWordWith cs' phr)

      return ""

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