module Main where 

import Data.Maybe
import Debug.Trace
import Data.Functor
import Data.List  
import qualified Data.Set as Set
import qualified Data.Map as Map
import System.Environment   
import System.Timeout
import Data.Char
import Data.Binary
  
import Utilities
import Wordlists
import Anagram
import HalfBenchmark
import Types
import Indicators
import Evaluation
import Dictionary
import Display
import LengthFunctions
import Abbreviation

import ClueBank
import Guardian
import Everyman

{-
main
  = do 
       -- GHC.Profiling.stopProfTimer
        print $  solve_clue 11
       --  GHC.Profiling.startProfTimer
        print $  is_wordlist_prefix "x"
       -- print $ {-# SCC "second" #-} (map solve clues)

-- Timeout is in microseconds
seconds
  = 1000000
dosolve x
  = timeout (20*seconds) $ do
            print $ solve x
-}

------------------ CLUE PARSING MECHANICS FUNCTIONS ------------------------

lowerCase :: Clue -> Clue
lowerCase (Clue (xs, n))
  = Clue (map toLower xs, n)

parse :: Clue -> [Parse]
parse (Clue (c, n))
  = parseWithIndicator ws n ++ parseWithoutIndicator ws n
  where
    ws = words c
    parseClueMem ws = Data.Maybe.fromJust (lookup ws parseTable)
    parseTable = [(ws', parseClue ws') | ws' <- substrings ws]
    
    parseWithoutIndicator :: [String] -> Int -> [Parse]
    parseWithoutIndicator ws n
      = [(unwords ws', p, n) | 
           (ws', ws'') <- split2' ws, 
           isInWordlist (unwords ws'), 
           p <- parseClueMem ws'']
    
    parseWithIndicator :: [String] -> Int -> [Parse]
    parseWithIndicator ws n
      = [(unwords ws', p, n) | 
           (ws', ws'', ws''') <- split3' ws,
           isInWordlist (unwords ws'), 
           isDefIndicator ws'', 
           p <- parseClueMem ws'''] 
    
    parseClue :: [String] -> [ParseTree]
    parseClue ws
      | length ws > 1 = parseWithConcat ws ++ parseWithoutConcat ws
      | otherwise     = parseWithoutConcat ws
    
    parseWithoutConcat :: [String] -> [ParseTree]
    parseWithoutConcat ws
      = parseSynonyms ws ++
        parseAnagrams ws ++
        parseHiddenWords ws ++
        parseInsertions ws ++
        parseSubtractions ws ++
        parseReversals ws ++
        parseFirstLetters ws ++
        parseLastLetters ws ++
        parsePartOf ws ++
        parseJuxtapositionIndicators ws 
    
    parseWithConcat :: [String] -> [ParseTree]
    parseWithConcat xs
      = map Concatenate ps
      where
        ps = concatMap (sequence . parseSubpart) (filter ((>1) . length) (partitions xs))
        parseSubpart part = [parseWithoutConcat subpart | subpart <- part]
    
    parseJuxtapositionIndicators :: [String] -> [ParseTree]
    parseJuxtapositionIndicators xs
      = if isJuxtapositionIndicator xs then [JuxtapositionIndicator xs] else []
    
    parseSynonyms :: [String] -> [ParseTree]
    parseSynonyms ws
      | hasValidLength pt n = [Synonym s]
      | otherwise           = [] 
      where
        s = unwords ws
        pt = Synonym s
    
    parseAnagrams :: [String] -> [ParseTree]
    parseAnagrams ws
      = [Anagram p p' | 
           (p, p') <- split2' ws, 
           isAnagramIndicator p,
           let pt = Anagram p p',
           hasValidLength pt n]
    parseInsertions :: [String] -> [ParseTree]
    parseInsertions ws
      = let parts = split3 ws
        in [Insertion ws' p p'' | 
             (ws, ws', ws'') <- parts, 
             isInsertionIndicator ws', 
             p <- parseClueMem ws, 
             p'' <- parseClueMem ws'',
             let pt = Insertion ws' p p'',
             hasValidLength pt n] ++ 
           [Insertion ws' p'' p | 
             (ws, ws', ws'') <- parts,
             isReverseInsertionIndicator ws', 
             p <- parseClueMem ws, 
             p'' <- parseClueMem ws'',
             let pt = Insertion ws' p'' p,
             hasValidLength pt n] 
    
    parseSubtractions :: [String] -> [ParseTree]
    parseSubtractions ws
      = let parts = split3 ws
        in [Subtraction ws' p p'' | 
             (ws, ws', ws'') <- parts,
             isSubtractionIndicator ws', 
             p <- parseClueMem ws, 
             p'' <- parseClueMem ws'',
             let pt = Subtraction ws' p p'',
             hasValidLength pt n] ++ 
           [Subtraction ws' p p'' | 
             (ws'', ws', ws) <- parts,
             isSubtractionIndicator ws', 
             p <- parseClueMem ws, 
             p'' <- parseClueMem ws'',
             let pt = Subtraction ws' p p'',
             hasValidLength pt n] 
    
    parseReversals :: [String] -> [ParseTree]
    parseReversals ws 
      = [Reversal ws p | 
          (ws, ws') <- split2' ws, 
          isRIndicator ws, 
          p <- parseClueMem ws',
          let pt = Reversal ws p,
          hasValidLength pt n]  
    
    parseHiddenWords :: [String] -> [ParseTree]
    parseHiddenWords ws
      = [HiddenWord ws ws' | 
          (ws, ws') <- split2 ws, 
          isHWIndicator ws,
          let pt = HiddenWord ws ws',
          hasValidLength pt n]
    
    parseFirstLetters :: [String] -> [ParseTree]
    parseFirstLetters ws
      = [FirstLetter ws ws' | 
          (ws, ws') <- split2' ws,
          isFLIndicator ws,
          let pt = FirstLetter ws ws',
          hasValidLength pt n]
    
    parseLastLetters :: [String] -> [ParseTree]
    parseLastLetters ws
      = [LastLetter ws ws' | 
          (ws, ws') <- split2' ws,
          isLLIndicator ws,
          let pt = LastLetter ws ws',
          hasValidLength pt n]
    
    parsePartOf :: [String] -> [ParseTree]
    parsePartOf ws
      = [PartOf ws p | 
          (ws, ws') <- split2' ws,
          isPartOfIndicator ws, 
          p <- map simplify (parseClueMem ws'),
          p /= Null,
          let pt = PartOf ws p,
          hasValidLength pt n]
    
simplify (Concatenate ts)
  = simpleConcat (map simplify ts)
simplify (Synonym ws)
  = Ident ws
simplify t
  = Null

simpleConcat ts
  | any (==Null) ts = Null
  | otherwise = Concatenate ts

-------------- COST EVALUATION -------------

parseCost (s, t, n)
  = cost t * (length_penalty s)
length_penalty ws
  = 60 + (length (words ws))

cost :: ParseTree -> Int
cost Null
  = 0
cost (Ident s)
  = 5
cost (Concatenate ts)
  = 20 * (length ts) + sum (map cost ts) 
cost (Anagram ind strings)
  = 10
cost (HiddenWord ind strings)
  = 40
cost (Insertion ind t1 t2)
  = 10 + cost t1 + cost t2 
cost (Subtraction ind t1 t2)
  = 30 + cost t1 + cost t2
cost (Reversal ind t)
  = 10 + cost t
cost (Synonym string)
  = 80 * length (words string)
cost (FirstLetter ind strings)
  = 20
cost (LastLetter ind strings)
  = 20
cost (Juxtapose one two)
  = 150
cost (PartOf ind t)
  = 60 + cost t
cost (JuxtapositionIndicator p)
  = 0

--------------------------- EVALUATION ----------------------------

checkSynonyms :: [Answer] -> [Answer]
checkSynonyms
  = filter checkSynonym

checkValidWords ::  [Answer] -> [Answer]
checkValidWords
  = filter isValidWord

isValidWord :: Answer -> Bool
isValidWord (Answer x (y, z, n))
  = isInWordlist x 

sortByParseCost ts
  = zip [(0::Int)..] (map snd . sort . map (\x -> (parseCost x, x)) $ ts)

hasValidLength :: ParseTree -> Int -> Bool
hasValidLength clue n
  = (minLength clue <= n) && (maxLength clue >= n) 

checkSynonym :: Answer -> Bool
checkSynonym (Answer string (def, clue, n))
  = Set.member string (Set.fromList (synonyms def))  

solve
  = head' . checkSynonyms . checkValidWords . evaluate . sortByParseCost . parse . lowerCase

parses
  = sortByParseCost . parse . lowerCase

head' []
  = []
head' xs
  = [head xs]
------------- SAMPLE CLUES ------------

clue :: Int -> Clue
clue 1
  = Clue ("companion shredded corset",6) -- ESCORT
clue 2
  = Clue ("notice in flying coat", 6) -- JACKET 
clue 3
  = Clue ("companion found in oklahoma terminal", 4)
-- SLOW - number 27 in the parse list...
clue 4
  = Clue ("a new member returned a woman", 6)
clue 5
  = Clue ("pause at these i fancy", 8) -- Everyman 3526, clue 1   ["athetise","hesitate"] 
clue 6
  = Clue ("ankle was twisted in ballet", 8) -- Everyman 3526, clue 3
clue 7
  = Clue ("flyer needed by funfair manager", 6)
clue 8
  = Clue ("put food in this stuff on barge at sea", 9) 
clue 9
  = Clue ("notice supervisor is going nuts at first", 4)
clue 10
  = Clue ("animal is mistake crossing one river", 7)
clue 11
  = Clue ("maria not a fickle lover", 9)
clue 12
  = Clue ("hope for high praise", 6)  
clue 13
  = Clue ("Not fed partly twigged", 5)
clue 14 
  = Clue ("Messy bit of lung next to part of kempton", 7)
clue 15 
  = Clue ("berate without rodent insect", 3)
clue 16
  = Clue ("Man changing line around Gates head", 5)
clue 17 
  = Clue ("Animal returns to grass", 4)
clue 18 
  = Clue ("bums for deals without energy", 9)
clue 19 
  = Clue ("Liberal posh wearing platinum with fancy cars to give rich people", 10)

grid
  = [("companion shredded corset", "??1???"), ("notice in flying coat", "??0??")]


