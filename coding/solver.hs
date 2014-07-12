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



main = do print $  solve_clue 11
         -- print $ {-# SCC "second" #-} (map solve clues)
   


------------------ CLUE PARSING MECHANICS FUNCTIONS ------------------------

includeReversals xs = xs ++ [(snd(x),fst(x)) | x <- xs] 

twoParts xs = [(x,y) | [x,y] <- partitions xs]
threeParts xs = [(x,y,z) | [x,y,z] <- partitions xs]

partitions [] = [[]]
partitions (x:xs) = [[x]:p | p <- partitions xs] ++ [(x:ys):yss | (ys:yss) <- partitions xs]

parse :: Clue -> [Parse]
parse (Clue (xs, n)) = makeNoIndicatorDefs (words xs, n) ++ makIndicatorDefs (words xs, n)

makeNoIndicatorDefs :: ([String], Int) -> [Parse]
makeNoIndicatorDefs (xs, n) = let parts = twoParts xs
        in [DefNode (concatWithSpaces (fst part)) y' n| part <- includeReversals (parts), y' <- (expand (snd part) n)]

makIndicatorDefs :: ([String], Int) -> [Parse]
makIndicatorDefs (xs, n) = let parts = threeParts xs
        in [DefNode (concatWithSpaces x) z' n|  (x,y,z) <- (parts), isDefIndicator(y), z' <- (expand z n)] 
        ++ [DefNode (concatWithSpaces x) z' n|  (z,y,x) <- (parts), isDefIndicator(y), z' <- (expand z n)]

expand :: [String] -> Int -> [ClueTree]
expand ys n= (if length ys > 1 then makeConsListNodes ys n else [])
	++ (expandNoCons ys n)

expandNoCons :: [String] -> Int -> [ClueTree]
expandNoCons ys n = [Leaf (concatWithSpaces ys)] 
  ++ (if length ys == 1 then makeConsIndicatorNodes ys n else [])
  ++ (if length ys > 1 then makeAnagramNodes ys n else [] )
  ++ (if length ys > 1 then makeHiddenWordNodes ys n else [])
  ++ (if length ys > 2 then makeInsertionNodes ys n else [])
  ++ (if length ys > 1 then makeReversalNodes ys n else [])
  ++ (if length ys > 1 then makeFirstLetterNodes ys n else [])
  ++ (if length ys > 1 then makeLastLetterNodes ys n else [])
  ++ (if length ys > 1 then makePartialNodes ys n else [])

expandJustAbbreviations :: [String] -> Int -> [ClueTree]
expandJustAbbreviations ys n = [Leaf (concatWithSpaces ys)] 

lowercase :: Clue -> Clue
lowercase (Clue (xs, n)) = Clue (map toLower xs, n)



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


makeConsNodes :: [String] -> Int -> [ClueTree]
makeConsNodes xs n = let parts = twoParts xs
                   in concat [[ConsNode x' y' |x' <- (expand (fst part) n), y' <- (expand (snd part) n)] | part <- parts]  
makeConsListNodes :: [String] -> Int -> [ClueTree]
makeConsListNodes xs n = [ConsListNode xs | xs <- (concat [sequence [expandNoCons subpart n| subpart <- part] | part <- partitions xs, (length part) > 1])] --, (sum . map minLength) xs >= n]

makeConsIndicatorNodes :: [String] -> Int -> [ClueTree]
makeConsIndicatorNodes xs n = if isConsIndicator xs then [ConsIndicatorLeaf xs] else []

-- ANAGRAMS

makeAnagramNodes :: [String] -> Int -> [ClueTree]
makeAnagramNodes xs n = let parts = twoParts xs
                  in [AnagramNode (AIndicator x) y | (x,y) <- includeReversals(parts), isAnagramWord(x), (length . concat) y <= n] 

-- INSERTIONS
makeInsertionNodes :: [String] -> Int -> [ClueTree]
makeInsertionNodes xs n = let parts = threeParts xs
                  in [InsertionNode (IIndicator y) x' z' | (x,y,z) <- parts, isInsertionWord(y), x' <- (expand x n), z' <- (expand z n)] 
                  ++ [InsertionNode (IIndicator y) z' x' | (x,y,z) <- parts, isReverseInsertionWord(y), x' <- (expand x n), z' <- (expand z n)] 

-- SUBTRACTIONS
makeSubtractionNodes :: [String] -> Int -> [ClueTree]
makeSubtractionNodes xs n = let parts = threeParts xs
                  in [SubtractionNode (SIndicator y) x' z' | (x,y,z) <- parts, isInsertionWord(y), x' <- (expand x n), z' <- (expand z n)] 

-- REVERSALS
makeReversalNodes :: [String] -> Int -> [ClueTree]
makeReversalNodes xs n  = let parts = twoParts xs
                  in [ReversalNode (RIndicator x) y2 | (x,y) <- includeReversals(parts), isRIndicator(x), y2 <- (expand y n)]  

-- HIDDEN WORDS
makeHiddenWordNodes :: [String]  -> Int -> [ClueTree]
makeHiddenWordNodes xs n = let parts = twoParts xs
                  in [HiddenWordNode (HWIndicator x) y | (x,y) <- parts, isHWIndicator(x)] --, (length (concat y)) > n 

-- FIRST LETTERS
makeFirstLetterNodes :: [String]  -> Int -> [ClueTree]
makeFirstLetterNodes xs n = let parts = twoParts xs
                  in [FirstLetterNode (FLIndicator x) y | (x,y) <- includeReversals(parts), isFLIndicator(x), (length y) <= n]

-- LAST LETTERS
makeLastLetterNodes :: [String]  -> Int -> [ClueTree]
makeLastLetterNodes xs n = let parts = twoParts xs
                  in [LastLetterNode (LLIndicator x) y | (x,y) <- includeReversals(parts), isLLIndicator(x), (length y) <= n]

-- LAST LETTERS
makePartialNodes :: [String]  -> Int -> [ClueTree]
makePartialNodes xs n = let parts = twoParts xs
                  in [PartialNode (PartialIndicator x) y' | (x,y) <- includeReversals(parts), isPartialIndicator(x), y' <- (expand y n)]

--------------------- KNOWN LETTER CONSTRAINS ---------------------

fits :: String -> String -> Bool
fits [] [] = True
fits [] (y:ys) = False
fits (x:xs) [] = False
fits (x:xs) (y:ys) = if x=='?' then (fits xs ys) else 
                        if x==y then (fits xs ys) else
                          False

answerFits ::  String -> Answer -> Bool
answerFits fitstring (Answer x y)  = fits fitstring x

stripFits :: String -> [Answer] -> [Answer]
stripFits s = filter (answerFits s) 

--------------------------- EVALUATION ----------------------------

check_valid_words ::  [Answer] -> [Answer]
check_valid_words = filter check_valid_word

check_valid_word :: Answer -> Bool
check_valid_word (Answer x (DefNode y z n)) = isInWordlist x 

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

solve = head . check_synonyms . check_valid_words . constrain_lengths .  evaluate . sort_most_likely . parse . lowercase

solve_no_syn = head . sort_solved  . check_valid_words . constrain_lengths  . evaluate . sort_most_likely . parse . lowercase

solve_clue = solve . clue

 
clue :: Int -> Clue
clue 1 = Clue ("companion shredded corset",6) -- ESCORT
clue 2 = Clue ("notice in flying coat", 6) -- JACKET
clue 3 = Clue ("companion found in oklahoma terminal", 4)
clue 4 = Clue ("a new member returned a woman", 6)
clue 5 = Clue ("pause at these i fancy", 8) -- Everyman 3526, clue 1   ["athetise","hesitate"] 
clue 6 = Clue ("ankle was twisted in ballet", 8) -- Everyman 3526, clue 3
clue 7 = Clue ("flyer needed by funfair manager", 6)
clue 8 = Clue ("put food in this stuff on barge at sea", 9) -- Why doesn't this work?
clue 9 = Clue ("notice supervisor is going nuts at first", 4)
clue 10 = Clue ("animal makes mistake crossing one river", 7)
clue 11 = Clue ("maria not a fickle lover", 9)
clue 12 = Clue ("hope for high praise", 6)  

grid = [("companion shredded corset", "??1???"), ("notice in flying coat", "??0??")]



-- REGEX ((\d*)\s(.+)\s\((\d*)\)\n(.*)\n(.*))\n