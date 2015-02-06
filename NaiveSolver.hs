module Main where 

import Data.Functor
import Data.List  
import qualified Data.Set
import qualified Data.Map as Map
import System.Environment   
import System.Timeout
import Data.Char
import Data.Binary
  
import Wordlists
import Anagram
import Types
import Indicators
import NaiveEvaluation
import Dictionary
import Display
import Abbreviation

import ClueBank
import Guardian
import Everyman

main
  = do 
       -- GHC.Profiling.stopProfTimer
       print $  solve (clue 11)
       --  GHC.Profiling.startProfTimer
       -- print $  is_wordlist_prefix "x"
       -- print $ {-# SCC "second" #-} (map solve clues)

-- Timeout is in microseconds
seconds
  = 1000000
dosolve x
  = timeout (20*seconds) $ do
            print $ solve x


------------------ CLUE PARSING MECHANICS FUNCTIONS ------------------------

mirror2 :: [(a, a)] -> [(a, a)]
mirror2 xs
  = xs ++ [(y, x) | (x, y) <- xs] 

mirror3 :: [(a, a, a)] -> [(a, a, a)]
mirror3 xs
  = xs ++ [(z, y, x) | (x, y, z) <- xs] 

{-
partitions :: [a] -> [[[a]]]
partitions []
  = [[]]
partitions (x : xs)
  = [[x] : p | p <- partitions xs] ++ [(x : ys) : yss | (ys : yss) <- partitions xs]
-}

split2 :: [a] -> [([a], [a])]
split2 []
  = []
split2 [x]
  = []
split2 (x : xs)
  = ([x], xs) : [(x : xs', xs'') | (xs', xs'') <- split2 xs]

{-
split2 :: [a] -> [([a], [a])]
split2 xs
  = [(x, y) | [x, y] <- partitions xs]
-}

split3 :: [a] -> [([a], [a], [a])]
split3 []
  = []
split3 [x]
  = []
split3 (x : xs)
  = [([x], xs', xs'') | (xs', xs'') <- split2 xs] ++
    [(x : xs', xs'', xs''') | (xs', xs'', xs''') <- split3 xs]

{-
split3 :: [a] -> [([a], [a], [a])]
split3 xs
  = [(x, y, z) | [x, y, z] <- partitions xs]
-}

split2' :: [a] -> [([a], [a])]
split2' 
  = mirror2 . split2

split3' :: [a] -> [([a], [a], [a])]
split3' 
  = mirror3 . split3

lowercase :: Clue -> Clue
lowercase (Clue (xs, n))
  = Clue (map toLower xs, n)

isInWordlistTRUE :: a -> Bool
isInWordlistTRUE x
  = True

parse :: Clue -> [Parse]
parse (Clue (c, n))
  = [(unwords ws', p, n) | 
       (ws', ws'') <- split2' (words c), 
       p <- parseClue ws'' n]

parseClue :: [String] -> Int -> [ParseTree]
parseClue ws n
  = parseJuxtapositions ws n ++
    parseSynonyms ws n ++
    parseAnagrams ws n ++
    parseHiddenWords ws n ++
    parseInsertions ws n ++
    parseSubtractions ws n ++
    parseReversals ws n ++
    parseFirstLetters ws n ++
    parseLastLetters ws n ++
    parsePartOfs ws n ++
    parseJuxtapositionIndicators ws n

parseSynonyms :: [String] -> Int -> [ParseTree]
parseSynonyms ws n
  = [Synonym (unwords ws)] 

parseJuxtapositions :: [String] -> Int -> [ParseTree]
parseJuxtapositions words n
  = [Juxtapose p p' | 
       (ws, ws') <- split2 words,
       p <- parseClue ws n, 
       p' <- parseClue ws' n]

parseAnagrams :: [String] -> Int -> [ParseTree]
parseAnagrams words n
  = [Anagram ws ws' | 
       (ws, ws') <- split2' words, 
       length (concat ws') <= n,
       isAnagramIndicator ws]

parseInsertions :: [String] -> Int -> [ParseTree]
parseInsertions words n
  = let parts = split3 words
    in [Insertion ws' p p'' | 
         (ws, ws', ws'') <- parts, 
         isInsertionIndicator ws', 
         p <- parseClue ws n, 
         p'' <- parseClue ws'' n] ++ 
       [Insertion ws' p'' p | 
         (ws, ws', ws'') <- parts,
         isReverseInsertionIndicator ws', 
         p <- parseClue ws n, 
         p'' <- parseClue ws'' n] 

parseSubtractions :: [String] -> Int -> [ParseTree]
parseSubtractions words n
  = let parts = split3 words
    in [Subtraction ws' p p'' | 
         (ws, ws', ws'') <- parts,
         isSubtractionIndicator ws', 
         p <- parseClue ws n, 
         p'' <- parseClue ws'' n] ++ 
       [Subtraction ws' p p'' | 
         (ws'', ws', ws) <- parts,
         isSubtractionIndicator ws', 
         p <- parseClue ws n, 
         p'' <- parseClue ws'' n] 

parseReversals :: [String] -> Int -> [ParseTree]
parseReversals words n 
  = [Reversal ws p | 
      (ws, ws') <- split2' words, 
      isRIndicator ws, 
      p <- parseClue ws' n]  

parseHiddenWords :: [String]  -> Int -> [ParseTree]
parseHiddenWords words n
  = [HiddenWord ws ws' | 
      (ws, ws') <- split2 words, 
      isHWIndicator ws]

parseFirstLetters :: [String]  -> Int -> [ParseTree]
parseFirstLetters words n
  = [FirstLetter ws ws' | 
      (ws, ws') <- split2' words,
      isFLIndicator ws]

parseLastLetters :: [String]  -> Int -> [ParseTree]
parseLastLetters words n
  = [LastLetter ws ws' | 
      (ws, ws') <- split2' words,
      isLLIndicator ws]

parsePartOfs :: [String]  -> Int -> [ParseTree]
parsePartOfs words n
  = [PartOf ws p | 
      (ws, ws') <- split2' words,
      isPartOfIndicator ws, 
      p <- parseClue ws' n]

parseJuxtapositionIndicators :: [String] -> Int -> [ParseTree]
parseJuxtapositionIndicators words n
  | isJuxtapositionIndicator words = [JuxtapositionIndicator words] 
  | otherwise                      = []

--------------------------- EVALUATION ----------------------------

checkValidWords ::  [Answer] -> [Answer]
checkValidWords
  = filter isValidWord

isValidWord :: Answer -> Bool
isValidWord (Answer ans _)
  = isInWordlist ans 

checkLengths :: [Answer] -> [Answer]
checkLengths
  = filter checkLength

checkLength :: Answer -> Bool
checkLength (Answer ans (def, clue, n))
  = length ans == n

checkSynonyms :: [Answer] -> [Answer]
checkSynonyms
  = filter checkSynonym

checkSynonym :: Answer -> Bool
checkSynonym (Answer ans (def, clue, n))
  = Data.Set.member ans (Data.Set.fromList (synonyms def))  

--------------------------- SOLUTION ----------------------------

solve :: Clue -> [Answer]
solve
  = checkSynonyms . checkValidWords . checkLengths .  evaluate . parse . lowercase

clue :: Int -> Clue
clue 1
  = Clue ("companion shredded corset",6) -- ESCORT
clue 2
  = Clue ("notice in flying coat", 6) -- JACKET 
clue 3
  = Clue ("companion found in oklahoma terminal", 4)
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
  = Clue ("one alone", 4)

