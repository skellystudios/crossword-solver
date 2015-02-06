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

lowercase :: Clue -> Clue
lowercase (Clue (xs, n))
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

-- parseCons needs fixing
parseWithoutConcat :: [String] -> Int -> [ParseTree]
parseWithoutConcat ws n
  = parseSynonymNodes ws n ++
    parseAnagramNodes ws n ++
    parseHiddenWordNodes ws n ++
    parseInsertionNodes ws n ++
    parseSubtractionNodes ws n ++
    parseReversalNodes ws n ++
    parseFirstLetterNodes ws n ++
    parseLastLetterNodes ws n ++
    parsePartialNodes ws n ++
    if length ws == 1 then parseConsIndicatorNodes ws n else []

parseSynonymNodes :: [String] -> Int -> [ParseTree]
parseSynonymNodes ws n
  | null (synonyms s) = []
  | otherwise         = [SynonymNode s] 
  where
    s = unwords ws

parseWithConcat :: [String] -> Int -> [ParseTree]
parseWithConcat xs n
  = map ConcatNode ps
  where
    ps = concatMap (sequence . parseSubpart) (filter ((>1) . length) (partitions xs))
    parseSubpart part = [parseWithoutConcat subpart n | subpart <- part]

parseConsIndicatorNodes :: [String] -> Int -> [ParseTree]
parseConsIndicatorNodes xs n
  = if isConsIndicator xs then [ConsIndicatorNode xs] else []

-- Swapped length test with hasAnagram - faster!
parseAnagramNodes :: [String] -> Int -> [ParseTree]
parseAnagramNodes ws n
  = [AnagramNode p p' | 
       (p, p') <- split2' ws, 
       length (concat p') <= n,
       hasAnagram p]

parseInsertionNodes :: [String] -> Int -> [ParseTree]
parseInsertionNodes ws n
  = let parts = split3 ws
    in [InsertionNode ws' p p'' | 
         (ws, ws', ws'') <- parts, 
         isInsertionIndicator ws', 
         p <- parseClue ws n, 
         p'' <- parseClue ws'' n] ++ 
       [InsertionNode ws' p'' p | 
         (ws, ws', ws'') <- parts,
         isReverseInsertionIndicator ws', 
         p <- parseClue ws n, 
         p'' <- parseClue ws'' n] 

parseSubtractionNodes :: [String] -> Int -> [ParseTree]
parseSubtractionNodes ws n
  = let parts = split3 ws
    in [SubtractionNode ws' p p'' | 
         (ws, ws', ws'') <- parts,
         isSubtractionIndicator ws', 
         p <- parseClue ws n, 
         p'' <- parseClue ws'' n] ++ 
       [SubtractionNode ws' p p'' | 
         (ws'', ws', ws) <- parts,
         isSubtractionIndicator ws', 
         p <- parseClue ws n, 
         p'' <- parseClue ws'' n] 

parseReversalNodes :: [String] -> Int -> [ParseTree]
parseReversalNodes ws n 
  = [ReversalNode ws p | 
      (ws, ws') <- split2' ws, 
      isRIndicator ws, 
      p <- parseClue ws' n]  

parseHiddenWordNodes :: [String]  -> Int -> [ParseTree]
parseHiddenWordNodes ws n
  = [HiddenWordNode ws ws' | 
      (ws, ws') <- split2 ws, 
      isHWIndicator ws,
      length (concat ws') > n]

parseFirstLetterNodes :: [String]  -> Int -> [ParseTree]
parseFirstLetterNodes ws n
  = [FirstLetterNode ws ws' | 
      (ws, ws') <- split2' ws,
      isFLIndicator ws, 
      length ws' <= n]

parseLastLetterNodes :: [String]  -> Int -> [ParseTree]
parseLastLetterNodes ws n
  = [LastLetterNode ws ws' | 
      (ws, ws') <- split2' ws,
      isLLIndicator ws, 
      length ws' <= n]

parsePartialNodes :: [String]  -> Int -> [ParseTree]
parsePartialNodes ws n
  = [PartialNode ws p | 
      (ws, ws') <- split2' ws,
      isPartialIndicator ws, 
      p <- parseClue ws' n]


-------------- COST EVALUATION -------------

cost_parse (s, t, n)
  = cost t * (length_penalty s)
length_penalty ws
  = 60 + (length (words ws))   -- Magic constant here ) : 

cost :: ParseTree -> Int
cost (ConcatNode ts)
  = 20 * (length ts) + sum (map cost ts) 
cost (AnagramNode ind strings)
  = 10
cost (HiddenWordNode ind strings)
  = 40
cost (InsertionNode ind t1 t2)
  = 10 + cost t1 + cost t2  -- weight against complex insertions?
cost (SubtractionNode ind t1 t2)
  = 30 + cost t1 + cost t2
cost (ReversalNode ind t)
  = 10 + cost t
cost (SynonymNode string)
  = 80 * length (words string)
cost (FirstLetterNode ind strings)
  = 20
cost (LastLetterNode ind strings)
  = 20
cost (ConsNode one two)
  = 150
cost (PartialNode ind t)
  = 60 + cost t
cost (ConsIndicatorNode p)
  = 0



--------------------- KNOWN LETTER CONSTRAINS ---------------------

known_letter_fits :: String -> String -> Bool
known_letter_fits [] []
  = True
known_letter_fits [] (y : ys)
  = False
known_letter_fits (x : xs) []
  = False
known_letter_fits (x : xs) (y : ys)
  = if x=='?' then (known_letter_fits xs ys) else 
                        if x==y then (known_letter_fits xs ys) else
                          False

answerFits ::  String -> Answer -> Bool
answerFits fitstring (Answer x y) 
  = known_letter_fits fitstring x

stripFits :: String -> [Answer] -> [Answer]
stripFits s
  = filter (answerFits s) 

answerPart :: Answer -> String
answerPart (Answer x y)
  = x

check_answer_equals :: String -> [Answer] -> [Answer]
check_answer_equals y z
  = filter (\x -> answerPart x == y) z

--------------------------- EVALUATION ----------------------------

checkValidWords ::  [Answer] -> [Answer]
checkValidWords
  = filter isValidWord

isValidWord :: Answer -> Bool
isValidWord (Answer x (y, z, n))
  = isInWordlist x 

constrainParseLengths :: [Parse] -> [Parse]
constrainParseLengths
  = filter valid_parse_length

valid_parse_length :: Parse -> Bool
valid_parse_length (def, clue, n)
  = (minLength clue <= n) && (maxLength clue >= n) 


constrainLengths :: [Answer] -> [Answer]
constrainLengths
  = filter checkLength

checkLength :: Answer -> Bool
checkLength (Answer string (def, clue, n)) 
  = length string == n

checkSynonyms :: [Answer] -> [Answer]
checkSynonyms
  = filter checkSynonym

checkSynonym :: Answer -> Bool
checkSynonym (Answer string (def, clue, n))
  = Data.Set.member string (Data.Set.fromList (synonyms def))  

cost_solved x
  = if checkSynonym x then 0 else cost_parse (get_parse x)

sort_solved
  = map snd . sort . map (\x -> (cost_solved x, x))

sortByCost
  = map snd . sort . map (\x -> (cost_parse x, x))

possible_words
  = checkValidWords . constrainLengths  . evaluate . parse





solve
  = head' . checkSynonyms . checkValidWords . constrainLengths .  evaluate . sortByCost . constrainParseLengths . parse . lowercase




solve'
  = solve_no_syn_sorted

solve_no_syn_sorted
  = head' . sort_solved  . take 100 . solve_no_syn

solve_no_syn_unsorted
  = head'  . solve_no_syn

solve_no_syn
  = checkValidWords . constrainLengths  . evaluate . sortByCost . constrainParseLengths . parse . lowercase

solve_clue
  = solve . clue

head' :: [a] -> [a]
head' []    
  = []
head' (x : xs)
  = [x]

compare_clue (Clue (s,n)) (Clue (t,m))
  = compare n m 
 
x :: Clue2 -> Clue
x (Clue2 xs (n))
  = Clue (xs, n) 

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

grid
  = [("companion shredded corset", "??1???"), ("notice in flying coat", "??0??")]


-- Naive concat?
parseConsNodes :: [String] -> Int -> [ParseTree]
parseConsNodes xs n
  = let parts = split2 xs
    in concat [[ConsNode x' y' |x' <- (parseClue (fst part) n), y' <- (parseClue (snd part) n)] | part <- parts]  

-- REGEX ((\d*)\s(.+)\s\((\d*)\)\n(.*)\n(.*))\n
