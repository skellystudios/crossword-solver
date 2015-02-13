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

mirror2 :: [(a, a)] -> [(a, a)]
mirror2 xs
  = xs ++ [(y, x) | (x, y) <- xs] 

mirror3 :: [(a, a, a)] -> [(a, a, a)]
mirror3 xs
  = xs ++ [(z, y, x) | (x, y, z) <- xs] 

partitions :: [a] -> [[[a]]]
partitions []
  = [[]]
partitions (x : xs)
  = [[x] : p | p <- partitions xs] ++ [(x : ys) : yss | (ys : yss) <- partitions xs]

split2 :: [a] -> [([a], [a])]
split2 xs
  = [(x, y) | [x, y] <- partitions xs]

split3 :: [a] -> [([a], [a], [a])]
split3 xs
  = [(x, y, z) | [x, y, z] <- partitions xs]

split2' :: [a] -> [([a], [a])]
split2' 
  = mirror2 . split2

split3' :: [a] -> [([a], [a], [a])]
split3' 
  = mirror3 . split3

lowerCase :: Clue -> Clue
lowerCase (Clue (xs, n))
  = Clue (map toLower xs, n)

isInWordlistTRUE :: a -> Bool
isInWordlistTRUE x
  = True

parse :: Clue -> [Parse]
parse (Clue (c, n))
  = parseWithIndicator ws n ++ parseWithoutIndicator ws n
  where
    ws = words c

parseWithoutIndicator :: [String] -> Int -> [Parse]
parseWithoutIndicator ws n
  = [(unwords ws', p, n) | 
       (ws', ws'') <- split2' ws, 
       isInWordlistTRUE (unwords ws'), 
       p <- parseClue ws'' n]

parseWithIndicator :: [String] -> Int -> [Parse]
parseWithIndicator ws n
  = [(unwords ws', p, n) | 
       (ws', ws'', ws''') <- split3' ws,
       isInWordlistTRUE (unwords ws'), 
       isDefIndicator ws'', 
       p <- parseClue ws''' n] 

parseClue :: [String] -> Int -> [ParseTree]
parseClue ws n
  | length ws > 1 = parseWithConcat ws n ++ parseWithoutConcat ws n
  | otherwise     = parseWithoutConcat ws n

parseWithoutConcat :: [String] -> Int -> [ParseTree]
parseWithoutConcat ws n
  = parseSynonyms ws n ++
    parseAnagrams ws n ++
    parseHiddenWords ws n ++
    parseInsertions ws n ++
    parseSubtractions ws n ++
    parseReversals ws n ++
    parseFirstLetters ws n ++
    parseLastLetters ws n ++
    parsePartOf ws n ++
    parseJuxtapositionIndicators ws n 

parseSynonyms :: [String] -> Int -> [ParseTree]
parseSynonyms ws n
  | null (synonyms s) = []
  | otherwise         = [Synonym s] 
  where
    s = unwords ws

parseWithConcat :: [String] -> Int -> [ParseTree]
parseWithConcat xs n
  = map Concatenate ps
  where
    ps = concatMap (sequence . parseSubpart) (filter ((>1) . length) (partitions xs))
    parseSubpart part = [parseWithoutConcat subpart n | subpart <- part]

parseJuxtapositionIndicators :: [String] -> Int -> [ParseTree]
parseJuxtapositionIndicators xs n
  = if isJuxtapositionIndicator xs then [JuxtapositionIndicator xs] else []

parseAnagrams :: [String] -> Int -> [ParseTree]
parseAnagrams ws n
  = [Anagram p p' | 
       (p, p') <- split2' ws, 
       length (concat p') <= n,
       isAnagramIndicator p]

parseInsertions :: [String] -> Int -> [ParseTree]
parseInsertions ws n
  = let parts = split3 ws
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
parseSubtractions ws n
  = let parts = split3 ws
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
parseReversals ws n 
  = [Reversal ws p | 
      (ws, ws') <- split2' ws, 
      isRIndicator ws, 
      p <- parseClue ws' n]  

parseHiddenWords :: [String]  -> Int -> [ParseTree]
parseHiddenWords ws n
  = [HiddenWord ws ws' | 
      (ws, ws') <- split2 ws, 
      isHWIndicator ws,
      length (concat ws') > n]

parseFirstLetters :: [String]  -> Int -> [ParseTree]
parseFirstLetters ws n
  = [FirstLetter ws ws' | 
      (ws, ws') <- split2' ws,
      isFLIndicator ws, 
      length ws' <= n]

parseLastLetters :: [String]  -> Int -> [ParseTree]
parseLastLetters ws n
  = [LastLetter ws ws' | 
      (ws, ws') <- split2' ws,
      isLLIndicator ws, 
      length ws' <= n]

parsePartOf :: [String]  -> Int -> [ParseTree]
parsePartOf ws n
  = [PartOf ws p | 
      (ws, ws') <- split2' ws,
      isPartOfIndicator ws, 
      p <- map simplify (parseClue ws' n),
      notNull p]

notNull Null
  = False
notNull t
  = True

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

constrainLengths :: [Answer] -> [Answer]
constrainLengths
  = filter checkLength

checkLength :: Answer -> Bool
checkLength (Answer string (def, clue, n)) 
  = length string == n

isValidWord :: Answer -> Bool
isValidWord (Answer x (y, z, n))
  = isInWordlist x 

sortByParseCost
  = map snd . sort . map (\x -> (parseCost x, x))

constrainParseLengths :: [Parse] -> [Parse]
constrainParseLengths
  = filter hasValidLength

hasValidLength :: Parse -> Bool
hasValidLength (def, clue, n)
  = (minLength clue <= n) && (maxLength clue >= n) 

checkSynonym :: Answer -> Bool
checkSynonym (Answer string (def, clue, n))
  = Data.Set.member string (Data.Set.fromList (synonyms def))  

solve
  = head . checkSynonyms . checkValidWords . constrainLengths .  evaluate . sortByParseCost . constrainParseLengths . parse . lowerCase

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

grid
  = [("companion shredded corset", "??1???"), ("notice in flying coat", "??0??")]


