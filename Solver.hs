module Main where 

import Debug.Trace
import Data.List  
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Char
import Data.Either
  
import Types
import ManualData
import Databases
import Utilities
import Indicators
import Learn
import Evaluation
import LengthFunctions
import Prefixes

------------------ CLUE PARSING MECHANICS FUNCTIONS ------------------------

lowerCase :: Clue -> Clue
lowerCase (Clue (xs, n))
  = Clue (filter (not . isPunctuation) (map toLower xs), n)

parse :: Clue -> [Parse]
parse clue
  = parseWithIndicator ws n ++ parseWithoutIndicator ws n
  where
    Clue (c, n) = lowerCase clue
    ws = words c

parseWithoutIndicator :: [String] -> Int -> [Parse]
parseWithoutIndicator ws n
  = [(unwords ws', p, n) | 
       (ws', ws'') <- split2' ws, 
       isInWordlist (unwords ws'), 
       p <- parseClue ws'']

parseWithIndicator :: [String] -> Int -> [Parse]
parseWithIndicator ws n
  = [(unwords ws', p, n) | 
       (ws', ws'', ws''') <- split3' ws,
       isInWordlist (unwords ws'), 
       isDefIndicator ws'', 
       p <- parseClue ws'''] 

parseClue :: [String] -> [ParseTree]
parseClue [w]
  = parseWithoutConcat [w]
parseClue ws
  = parseWithoutConcat ws ++ parseWithConcat ws 

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
    parseJuxtapositions ws 

parseWithConcat :: [String] -> [ParseTree]
parseWithConcat xs
  = map Concatenate ps
  where
    ps = concatMap (sequence . parseSubpart) (filter ((>1) . length) (partitions xs))
    parseSubpart part = [parseWithoutConcat subpart | subpart <- part]

parseSynonyms :: [String] -> [ParseTree]
parseSynonyms ws
  = [Synonym (unwords ws)] 

parseAnagrams :: [String] -> [ParseTree]
parseAnagrams ws
  = [Anagram p p' | 
       (p, p') <- split2' ws, 
       isAnagramIndicator p]

parseInsertions :: [String] -> [ParseTree]
parseInsertions ws
  = let parts = split3 ws
    in [Insertion ws' p p'' | 
         (ws, ws', ws'') <- parts, 
         isInsertionIndicator ws', 
         p <- parseClue ws, 
         p'' <- parseClue ws''] ++ 
       [Insertion ws' p'' p | 
         (ws, ws', ws'') <- parts,
         isReverseInsertionIndicator ws', 
         p <- parseClue ws, 
         p'' <- parseClue ws''] 

parseSubtractions :: [String] -> [ParseTree]
parseSubtractions ws
  = let parts = split3 ws
    in [Subtraction ws' p p'' | 
         (ws, ws', ws'') <- parts,
         isSubtractionIndicator ws', 
         p <- parseClue ws, 
         p'' <- parseClue ws''] ++ 
       [Subtraction ws' p p'' | 
         (ws'', ws', ws) <- parts,
         isSubtractionIndicator ws', 
         p <- parseClue ws, 
         p'' <- parseClue ws''] 

parseReversals :: [String] -> [ParseTree]
parseReversals ws 
  = [Reversal ws p | 
      (ws, ws') <- split2' ws, 
      isReversalIndicator ws, 
      p <- parseClue ws']  

parseHiddenWords :: [String] -> [ParseTree]
parseHiddenWords ws
  = [HiddenWord ws ws' | 
      (ws, ws') <- split2 ws, 
      isHiddenWordIndicator ws]

parseFirstLetters :: [String] -> [ParseTree]
parseFirstLetters ws
  = [FirstLetter ws ws' | 
      (ws, ws') <- split2' ws,
      isFirstLetterIndicator ws]

parseLastLetters :: [String] -> [ParseTree]
parseLastLetters ws
  = [LastLetter ws ws' | 
      (ws, ws') <- split2' ws,
      isLastLetterIndicator ws]

parsePartOf :: [String] -> [ParseTree]
parsePartOf ws
  = [PartOf ws p | 
      (ws, ws') <- split2' ws,
      isPartOfIndicator ws, 
      p <- map simplify (parseClue ws'),
      p /= Null]

parseJuxtapositions :: [String] -> [ParseTree]
parseJuxtapositions ws
  = let parts = split3 ws
    in [Juxtaposition ws' p p'' | 
         (ws, ws', ws'') <- parts,
         isJuxtapositionIndicator ws', 
         p <- parseClue ws, 
         p'' <- parseClue ws''] ++
       [Juxtaposition ws' p'' p | 
         (ws'', ws', ws) <- parts,
         isReverseJuxtapositionIndicator ws', 
         p <- parseClue ws, 
         p'' <- parseClue ws''] 

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
cost (PartOf ind t)
  = 60 + cost t
cost (Juxtaposition ind t1 t2)
  = cost (Concatenate [t1, t2])

--------------------------- EVALUATION ----------------------------

checkSynonyms :: [Answer] -> Either [Answer] [Answer]
checkSynonyms answers
  | null sols = Left answers
  | otherwise = Right sols
  where
    sols = filter checkSynonym answers

checkSynonym :: Answer -> Bool
checkSynonym (Answer string (def, clue, n))
  = Set.member string (Set.fromList (synonyms def))  

checkValidWords ::  [Answer] -> [Answer]
checkValidWords
  = filter isValidWord

isValidWord :: Answer -> Bool
isValidWord (Answer x (y, z, n))
  = isInWordlist x 

sortByParseCost ts
  = zip (zip [(0::Int)..] (repeat (length ts))) (map snd . sort . map (\x -> (parseCost x, x)) $ ts)

constrainParseLengths :: Clue -> SynonymTable -> [Parse] -> [Parse]
constrainParseLengths (Clue (c, n)) synTable ts
  = filter hasValidLength ts
  where 
    hasValidLength (_, clue, _)
      = (minLength synTable clue <= n) && (maxLength synTable clue >= n) 

makeTable s
  = [(w, (minimum ns, maximum ns)) | w <- substrings (words s), let ns = map length (synonyms (unwords w))]

solve c
  = (head' . checkSynonyms . evaluate synTable . sortByParseCost .
     constrainParseLengths c synTable . parse) c
  where
    Clue (s, _) = lowerCase c
    synTable = makeTable s

parses c
  = (sortByParseCost . constrainParseLengths c synTable . parse) c
  where
    Clue (s, _) = lowerCase c
    synTable = [(w, (minimum ns, maximum ns)) | w <- substrings (words s), let ns = map length (synonyms (unwords w))]

table c@(Clue (s, n))
  = [(w, (minimum ns, maximum ns)) | w <- substrings (words s), let ns = map length (synonyms (unwords w))]

head' (Left as)
  = ("UNSOLVED", as)
head' (Right sols)
  = ("SOLVED", [head sols])

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
  = Clue ("bums mix for deals without energy", 9)
clue 19 
  = Clue ("liberal posh wearing platinum with fancy cars to give rich people", 10)
clue 20
  = Clue ("indications show surprising gains for example after recovery",7)
clue 21 
  = Clue ("Scholarly head of languages brought in", 7)
clue 22 
  = Clue ("A zebra dropping guts, munching bitter plant",6)
grid
  = [("companion shredded corset", "??1???"), ("notice in flying coat", "??0??")]


