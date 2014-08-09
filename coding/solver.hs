module Main where 

import Data.List  
import qualified Data.Set
import qualified Data.Map as Map
import System.Environment   
import Data.Char
import Data.Binary
  
import Wordlists
import Anagram
import Benchmarks
import Types
import Utils
import Indicators
import Evaluation
import Dictionary
import Display
import LengthFunctions
import Abbreviation


import Guardian
import Everyman

-- import GHC

-- import Control.Deepseq
--  \(Clue \("[\w\s]*",(\d[\d,]+)\), "\w*"\),
-- \(Clue \("[\w—\s]*",\d,[\d,]*\), "\w*"\),

-- ghc -prof -auto-all solver.hs
-- ghc-pkg list base

{-

SELECT CONCAT("\"",WORD,"\" = [", GROUP_CONCAT( CONCAT("\"",abbreviation,"\"")),"]") 
FROM (SELECT DISTINCT word, abbreviation FROM abbreviations) ab
GROUP BY word;

:set +s

-}


{-  USE NULL INSTEAD OF LENGTH == 0  -}
  

  {-

  Removal of first and last letters
  Removal of central letter(s)
  Removal of alternate letters

  -}


main = do 
       -- GHC.Profiling.stopProfTimer
        print $  solve_clue 11
      --  GHC.Profiling.startProfTimer
        print $  is_wordlist_prefix "x"
         -- print $ {-# SCC "second" #-} (map solve clues)
   


------------------ CLUE PARSING MECHANICS FUNCTIONS ------------------------

includeReversals xs = xs ++ [(snd(x),fst(x)) | x <- xs] 

twoParts xs = [(x,y) | [x,y] <- partitions xs]
threeParts xs = [(x,y,z) | [x,y,z] <- partitions xs]

partitions [] = [[]]
partitions (x:xs) = [[x]:p | p <- partitions xs] ++ [(x:ys):yss | (ys:yss) <- partitions xs]

lowercase :: Clue -> Clue
lowercase (Clue (xs, n)) = Clue (map toLower xs, n)


parse :: Clue -> [Parse]
parse (Clue (xs, n)) = parseWithIndicator (words xs, n) ++ parseWithoutIndicator (words xs, n)

parseWithoutIndicator :: ([String], Int) -> [Parse]
parseWithoutIndicator (xs, n) = let parts = twoParts xs
        in [DefNode (concatWithSpaces (fst part)) y' n| part <- includeReversals (parts), isInWordlist(concatWithSpaces (fst part)), y' <- (parseClue (snd part) n)]

parseWithIndicator :: ([String], Int) -> [Parse]
parseWithIndicator (xs, n) = let parts = threeParts xs
        in [DefNode (concatWithSpaces x) z' n|  (x,y,z) <- (parts), isInWordlist(concatWithSpaces x), isDefIndicator(y), z' <- (parseClue z n)] 
        ++ [DefNode (concatWithSpaces x) z' n|  (z,y,x) <- (parts), isInWordlist(concatWithSpaces x), isDefIndicator(y), z' <- (parseClue z n)]

parseClue :: [String] -> Int -> [ClueTree]
parseClue ys n= (if length ys > 1 then parseConsNodes ys n else [])
	++ (parseWithoutConcat ys n)

parseWithoutConcat :: [String] -> Int -> [ClueTree]
parseWithoutConcat ys n = [Leaf (concatWithSpaces ys)] 
  ++ (if length ys == 1 then parseConsIndicatorNodes ys n else [])
  ++ (if length ys > 1 then parseAnagramNodes ys n else [] )
  ++ (if length ys > 1 then parseHiddenWordNodes ys n else [])
  ++ (if length ys > 2 then parseInsertionNodes ys n else [])
  ++ (if length ys > 2 then parseSubtractionNodes ys n else [])
  ++ (if length ys > 1 then parseReversalNodes ys n else [])
  ++ (if length ys > 1 then parseFirstLetterNodes ys n else [])
  ++ (if length ys > 1 then parseLastLetterNodes ys n else [])
  ++ (if length ys > 1 then parsePartialNodes ys n else [])


parseJustAbbreviations :: [String] -> Int -> [ClueTree]
parseJustAbbreviations ys n = [Leaf (concatWithSpaces ys)] 

parseConsNodes :: [String] -> Int -> [ClueTree]
parseConsNodes xs n = let parts = twoParts xs
                   in concat [[ConsNode x' y' |x' <- (parseClue (fst part) n), y' <- (parseClue (snd part) n)] | part <- parts]  
parseConcatNodes :: [String] -> Int -> [ClueTree]
parseConcatNodes xs n = [ConsListNode ys | ys <- (concat [sequence [parseWithoutConcat subpart n| subpart <- part] | part <- partitions xs])] --, (length part) > 1])] --, (sum . (map minLength) $ ys) >= n, (sum . (map maxLength) $ ys) <= n ] --, (sum . map minLength) xs >= n]

parseConsIndicatorNodes :: [String] -> Int -> [ClueTree]
parseConsIndicatorNodes xs n = if isConsIndicator xs then [ConsIndicatorLeaf xs] else []

parseAnagramNodes :: [String] -> Int -> [ClueTree]
parseAnagramNodes xs n = let parts = twoParts xs
                  in [AnagramNode (AIndicator x) y | (x,y) <- includeReversals(parts), isAnagramWord(x)] --, (length . concat) y <= n] 

parseInsertionNodes :: [String] -> Int -> [ClueTree]
parseInsertionNodes xs n = let parts = threeParts xs
                  in [InsertionNode (IIndicator y) x' z' | (x,y,z) <- parts, isInsertionWord(y), x' <- (parseClue x n), z' <- (parseClue z n)] 
                  ++ [InsertionNode (IIndicator y) z' x' | (x,y,z) <- parts, isReverseInsertionWord(y), x' <- (parseClue x n), z' <- (parseClue z n)] 

parseSubtractionNodes :: [String] -> Int -> [ClueTree]
parseSubtractionNodes xs n = let parts = threeParts xs
                  in [SubtractionNode (SIndicator y) x' z' | (x,y,z) <- parts, isSubtractionWord(y), x' <- (parseClue x n), z' <- (parseClue z n)] 
                  ++ [SubtractionNode (SIndicator y) x' z' | (z,y,x) <- parts, isSubtractionWord(y), x' <- (parseClue x n), z' <- (parseClue z n)] 

parseReversalNodes :: [String] -> Int -> [ClueTree]
parseReversalNodes xs n  = let parts = twoParts xs
                  in [ReversalNode (RIndicator x) y2 | (x,y) <- includeReversals(parts), isRIndicator(x), y2 <- (parseClue y n)]  

parseHiddenWordNodes :: [String]  -> Int -> [ClueTree]
parseHiddenWordNodes xs n = let parts = twoParts xs
                  in [HiddenWordNode (HWIndicator x) y | (x,y) <- parts, isHWIndicator(x)] --, (length (concat y)) > n 

parseFirstLetterNodes :: [String]  -> Int -> [ClueTree]
parseFirstLetterNodes xs n = let parts = twoParts xs
                  in [FirstLetterNode (FLIndicator x) y | (x,y) <- includeReversals(parts), isFLIndicator(x)] -- (length y) <= n

parseLastLetterNodes :: [String]  -> Int -> [ClueTree]
parseLastLetterNodes xs n = let parts = twoParts xs
                  in [LastLetterNode (LLIndicator x) y | (x,y) <- includeReversals(parts), isLLIndicator(x)] -- (length y) <= n

parsePartialNodes :: [String]  -> Int -> [ClueTree]
parsePartialNodes xs n = let parts = twoParts xs
                  in [PartialNode (PartialIndicator x) y' | (x,y) <- includeReversals(parts), isPartialIndicator(x), y' <- (parseClue y n)]

-------------- COST EVALUATION -------------

cost_parse (DefNode s tree n) = cost tree * (length_penalty s)
length_penalty ws = 60 + (length (words ws))   -- Magic constant here ):

cost :: ClueTree -> Int
cost (ConsListNode trees) = 20 * (length trees) + sum (map cost trees) 
cost (AnagramNode ind strings) = 10
cost (HiddenWordNode ind strings) = 40
cost (InsertionNode ind tree1 tree2) = 40 + cost tree1 + cost tree2  -- weight against complex insertions?
cost (SubtractionNode ind tree1 tree2) = 30 + cost tree1 + cost tree2
cost (ReversalNode ind tree) = 20 + cost tree
cost (Leaf string) = 80 * length (words string)
cost (FirstLetterNode ind strings) = 20
cost (LastLetterNode ind strings) = 20
cost (ConsNode one two) = 150
cost (PartialNode ind tree) = 60 + cost tree
cost (ConsIndicatorLeaf xs) = 0



--------------------- KNOWN LETTER CONSTRAINS ---------------------

known_letter_fits :: String -> String -> Bool
known_letter_fits [] [] = True
known_letter_fits [] (y:ys) = False
known_letter_fits (x:xs) [] = False
known_letter_fits (x:xs) (y:ys) = if x=='?' then (known_letter_fits xs ys) else 
                        if x==y then (known_letter_fits xs ys) else
                          False

answerFits ::  String -> Answer -> Bool
answerFits fitstring (Answer x y)  = known_letter_fits fitstring x

stripFits :: String -> [Answer] -> [Answer]
stripFits s = filter (answerFits s) 

answerPart :: Answer -> String
answerPart (Answer x y) = x

--------------------------- EVALUATION ----------------------------

check_valid_words ::  [Answer] -> [Answer]
check_valid_words = filter check_valid_word

check_valid_word :: Answer -> Bool
check_valid_word (Answer x (DefNode y z n)) = isInWordlist x 

constrain_parse_lengths :: [Parse] -> [Parse]
constrain_parse_lengths = filter constrain_parse_length

constrain_parse_length :: Parse -> Bool
constrain_parse_length (DefNode def clue n) = (minLength clue <= n) && (maxLength clue >= n) 


constrain_lengths :: [Answer] -> [Answer]
constrain_lengths = filter constrain_length

constrain_length :: Answer -> Bool
constrain_length (Answer string (DefNode def clue n))  = length (string) == n

check_synonyms :: [Answer] -> [Answer]
check_synonyms = filter check_synonym

check_synonym :: Answer -> Bool
check_synonym (Answer string (DefNode def clue n)) = Data.Set.member string (Data.Set.fromList (syn def))  

cost_solved x = if check_synonym x then 0 else cost_parse (get_parse x)

sort_solved = map (snd) . sort . map (\x -> (cost_solved x, x))

sort_most_likely = map (snd) . sort . map (\x -> (cost_parse x, x))

possible_words = check_valid_words . constrain_lengths  . evaluate . parse





solve = head' . check_synonyms . check_valid_words . constrain_lengths .  evaluate . sort_most_likely . constrain_parse_lengths . parse . lowercase

naive = choose . evaluate . parse

-- choose :: [Answer] -> Answer
choose = head . filter valid 




valid :: Answer -> Bool
valid (Answer ans (DefNode def pt len)) = (length ans == len)  && (is_syn ans def)


num_parses = length . parse

num_evals = length . evaluate . parse

get_everyman = everyman

solve' = solve_no_syn_sorted

solve_no_syn_sorted = head' . sort_solved  . take 100 . solve_no_syn

solve_no_syn_unsorted = head'  . solve_no_syn

solve_no_syn = check_valid_words . constrain_lengths  . evaluate . sort_most_likely . constrain_parse_lengths . parse . lowercase

solve_clue = solve . clue

head' :: [a] -> [a]
head' []     = []
head' (x:xs) = [x]

compare_clue (Clue (s,n)) (Clue (t,m)) = compare n m 
 
clue :: Int -> Clue
clue 1 = Clue ("companion shredded corset",6) -- ESCORT
clue 2 = Clue ("notice in flying coat", 6) -- JACKET 
clue 3 = Clue ("companion found in oklahoma terminal", 4)
clue 4 = Clue ("a new member returned a woman", 6)
clue 5 = Clue ("pause at these i fancy", 8) -- Everyman 3526, clue 1   ["athetise","hesitate"] 
clue 6 = Clue ("ankle was twisted in ballet", 8) -- Everyman 3526, clue 3
clue 7 = Clue ("flyer needed by funfair manager", 6)
clue 8 = Clue ("put food in this stuff on barge at sea", 9) 
clue 9 = Clue ("notice supervisor is going nuts at first", 4)
clue 10 = Clue ("animal is mistake crossing one river", 7)
clue 11 = Clue ("maria not a fickle lover", 9)
clue 12 = Clue ("hope for high praise", 6)  

grid = [("companion shredded corset", "??1???"), ("notice in flying coat", "??0??")]



-- REGEX ((\d*)\s(.+)\s\((\d*)\)\n(.*)\n(.*))\n