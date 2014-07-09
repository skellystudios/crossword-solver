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


 



--- TOOD SECTION

-- DATA
-- Get a bunch of benchmark clues (100K?)
-- Find all indicator words from the internet pls

-- LARGE-SCALE STRUCTURAL STUFF
-- Make a testing suite (separate module)
-- Structure this whole shebang into modules
-- Make 'knowledge' an extra input into syn
-- Replace 'Cons' with Concat everywhere
-- Rename the evaluation functions to a consistent naming structure(?)
-- Multiple word clues!

-- WHOLE GRID SOLVING
-- Whole grid solving and representation
-- Don't remove non-valid words and non-synonyms, just score them worse
-- Allow us to ask for the top n answers, and divide score by sum(score) to give probabilities
-- Repeated function application to solve whole grid
-- Create a data structure for that grid

-- IMPORTANT FOR CORRECTNESS
-- find_in DOESNT'T WORK!!!
-- Do a thing wherein we deal with the problem with leaf nodes not evaluating to anything. THIS IS WHERE I CAN USE A MAYBE A MONAD
-- Ditto with invalid subtractions. This is pretty important!
-- Sometimes need to use synonymns when doing anagrams ??? Maybe anagram subtypes needs to be a special type of subtree

-- EFFICIENCIES AND UPGRADES
-- Improve subtraction clues evaluation mechanism
-- Pre-process anagrams and pass them through (??? WILL THIS BE USEFUL - RUN SOME TESTS)
-- Check beginning of words while processing to check for valid words - can we pass partial words down the cons chain? (i.e. if we've already generated 3 letters from the first one, then give words minus the first three letters)
-- Conditional eval on insertion node should be smarter 

-- WRITE UP / RESEARCH
-- Write something about reverses and trees for output - and implications for type system.
-- Garbage Collection, write about it.
-- Concurrency - make it fun!
-- Look up Suffix trees to compress thesaurus
-- Could do some sort of statistical evaluation to determine the cost function. Or, like, machine learn it?

--- DISPLAY FUNCTIONS
-- A function that makes a printed clue markup version (Clue -> String) [ah, but hard, as we don't create a total parse tree including subs etc, at the end]

main = do print (solve_clue 11)
          
showDef :: Parse -> String
showDef (DefNode d tree n) = "Definition: " ++ show d ++ " \n" ++ showTree tree 1 ++ " \n\n" 

showTree :: ClueTree -> Int -> String
showTree (ConsNode x y) n = spaces n ++ showTreeL x (n+1) ++ showTreeL y (n+1)
showTree (ConsListNode xs) n = spaces n ++ "Cons \n" ++ concat (map (\x -> (showTreeL x (n+1))) xs)
showTree (AnagramNode (AIndicator anagrinds) strings) n = spaces n ++ "Anagram (" ++ concatWithSpaces anagrinds ++ ") " ++ concat strings
showTree (InsertionNode (IIndicator ind) t1 t2) n = spaces n ++ "Insert ("++ concatWithSpaces ind++") \n" ++ showTreeL t1 (n+1) ++ spaces n ++ "into" ++ " \n" ++ showTreeL t2 (n+1)
showTree (SubtractionNode (SIndicator ind) t1 t2) n = spaces n ++ "Subtract ("++ concatWithSpaces ind++") \n" ++ showTreeL t1 (n+1) ++ spaces n ++ "from" ++ " \n" ++ showTreeL t2 (n+1)
showTree x n = spaces n ++ show x 


spaces 0 = ""
spaces n = "    " ++ spaces (n-1)

showTreeL x n = showTree x n ++ "\n"

print_this def = (putStr . showDef) def

print_all defs = mapM print_this defs

answer_string (Answer x y) = x
just_answers = map answer_string


------------------ CLUE PARSING MECHANICS FUNCTIONS ------------------------

includeReversals xs = xs ++ [(snd(x),fst(x)) | x <- xs] 

twoParts xs = map (\x -> (head x, (head . tail) x)) (nPartitions 2 xs)
threeParts xs = map (\x -> (head x, (head . tail) x , (head . tail . tail) x)) (nPartitions 3 xs)

concatWithSpaces (x:[]) = x
concatWithSpaces (x:xs) = x ++ " " ++ concatWithSpaces xs

nPartitions :: Int -> ([String] -> [[[String]]])
nPartitions n xs = [xss | xss <- partitions xs, length xss == n]

partitions [] = [[]]
partitions (x:xs) = [[x]:p | p <- partitions xs] ++ [(x:ys):yss | (ys:yss) <- partitions xs]


parse :: Clue -> [Parse]
parse (Clue (xs, n)) = makeNoIndicatorDefs (words xs, n) ++ makIndicatorDefs (words xs, n)

makeNoIndicatorDefs :: ([String], Int) -> [Parse]
makeNoIndicatorDefs (xs, n) = let parts = twoParts xs
        in concat [[DefNode (concatWithSpaces (fst part)) y' n| y' <- (expand (snd part) n), isInWordlist (concatWithSpaces (fst part)) ] | part <- includeReversals (parts)]

makIndicatorDefs :: ([String], Int) -> [Parse]
makIndicatorDefs (xs, n) = let parts = threeParts xs
        in concat [[DefNode (concatWithSpaces x) z' n| z' <- (expand z n)] | (x,y,z) <- (parts), isDefIndicator(y), isInWordlist (concatWithSpaces x) ] 
        ++ concat [[DefNode (concatWithSpaces x) z' n| z' <- (expand z n)] | (z,y,x) <- (parts), isDefIndicator(y), isInWordlist (concatWithSpaces x) ]


isDefIndicator ["in"] = True
isDefIndicator ["for"] = True
isDefIndicator ["is"] = True
isDefIndicator ["makes"] = True
isDefIndicator _ = False

expand :: [String] -> Int -> [ClueTree]
expand ys n= (if length ys > 1 then makeConsListNodes ys n else [])
	++ (expandNoCons ys n)

expandNoCons :: [String] -> Int -> [ClueTree]
expandNoCons ys n = [Leaf (concatWithSpaces ys)] 
  ++ (if length ys == 1 then makeConsIndicatorNodes ys n else [])
  ++ (if length ys > 1 then makeAnagramNodes ys n else [] )
  ++ (if length ys > 1 then makeHiddenWordNodes ys n else [])
  ++ (if length ys > 2 then makeInsertionNodes ys n else [])
-- ++ (if length ys > 2 then makeConsIndicatorNodes ys n else [])
  ++ (if length ys > 1 then makeReversalNodes ys n else [])
  ++ (if length ys > 1 then makeFirstLetterNodes ys n else [])
  ++ (if length ys > 1 then makeLastLetterNodes ys n else [])
  ++ (if length ys > 1 then makePartialNodes ys n else [])

expandJustAbbreviations :: [String] -> Int -> [ClueTree]
expandJustAbbreviations ys n = [Leaf (concatWithSpaces ys)] 


lowercase :: Clue -> Clue
lowercase (Clue (xs, n)) = Clue (map toLower xs, n)


------------ LENGTH EVALUATION FUNCTIONS -----------------

minLength (ConsListNode trees) = (sum . map minLength) trees
minLength (AnagramNode ind strings) = (length . concat) strings
minLength (HiddenWordNode ind strings) = 2
minLength (InsertionNode ind tree1 tree2) = (minLength tree1) + (minLength tree2)
minLength (SubtractionNode ind tree1 tree2) = maximum[(minLength tree1) - (maxLength tree2),3]
minLength (ReversalNode ind tree) = minLength tree
minLength (Leaf string) = let x = minimum ( map length (string : syn string)) in x
minLength (FirstLetterNode ind strings) = length strings
minLength (LastLetterNode ind strings) = length strings
minLength (ConsNode one two) = minLength one + minLength two
minLength (PartialNode ind tree) = 1
minLength (ConsIndicatorLeaf xs) = 0


maxLength (ConsListNode trees) = (sum . map maxLength) trees
maxLength (AnagramNode ind strings) = (length . concat) strings
maxLength (HiddenWordNode ind strings) = (length strings) - 2
maxLength (InsertionNode ind tree1 tree2) = (maxLength tree1) + (maxLength tree2)
maxLength (SubtractionNode ind tree1 tree2) = minimum[(maxLength tree1) - (minLength tree2),3]
maxLength (ReversalNode ind tree) = maxLength tree
maxLength (Leaf string) = let x = maximum ( map length (string : syn string)) in x
maxLength (FirstLetterNode ind strings) = length strings
maxLength (LastLetterNode ind strings) = length strings
maxLength (ConsNode one two) = maxLength one + maxLength two
maxLength (PartialNode ind tree) = (maxLength tree) - 1
maxLength (ConsIndicatorLeaf xs) = 0

-------------- COST EVALUATION -------------

cost_parse (DefNode s tree n) = cost tree

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

-- Make cons indicator and then filter them out afterwards

makeConsIndicatorNodes :: [String] -> Int -> [ClueTree]
makeConsIndicatorNodes xs n = if isConsIndicator xs then [ConsIndicatorLeaf xs] else []

-- makeConsIndicatorNodes xs n = let parts = threeParts xs
--                 in concat [[ConsNode x' y' |(x, ind, y) <- parts, x' <- (expand x n), y' <- (expand y n), isConsIndicator(ind)] | part <- parts]  


isConsIndicator ["on"] = True
isConsIndicator _ = False

-- SUCH THAT sum(map (minLength) xs) <= clueLength and sum(map (maxLength) xs) >= clue

-- ANAGRAMS

makeAnagramNodes :: [String] -> Int -> [ClueTree]
makeAnagramNodes xs n = let parts = twoParts xs
                  in [AnagramNode (AIndicator x) y | (x,y) <- includeReversals(parts), isAnagramWord(x), (length . concat) y <= n] 

isAnagramWord :: [String] -> Bool
isAnagramWord xs = Data.Set.member (concatWithSpaces xs) anagramIndicators

anagrams :: String -> [String]
anagrams [] = [[]]
anagrams xs = [x:ys | x<-xs, ys <- anagrams(delete x xs)]

-- INSERTIONS
makeInsertionNodes :: [String] -> Int -> [ClueTree]
makeInsertionNodes xs n = let parts = threeParts xs
                  in [InsertionNode (IIndicator y) x' z' | (x,y,z) <- parts, isInsertionWord(y), x' <- (expand x n), z' <- (expand z n)] 
                  ++ [InsertionNode (IIndicator y) z' x' | (x,y,z) <- parts, isReverseInsertionWord(y), x' <- (expand x n), z' <- (expand z n)] 

insertInto :: String -> String -> [String] 
insertInto xs [] = [xs]
insertInto xs (y:ys) = [y:(xs ++ ys)] ++ (map ((:) y) (insertInto xs ys)) 

isInsertionWord ["in"] = True
isInsertionWord _ = False

isReverseInsertionWord ["crossing"] = True
isReverseInsertionWord ["contains"] = True
isReverseInsertionWord ["around"] = True
isReverseInsertionWord ["about"] = True
isReverseInsertionWord _ = False

-- SUBTRACTIONS
makeSubtractionNodes :: [String] -> Int -> [ClueTree]
makeSubtractionNodes xs n = let parts = threeParts xs
                  in [InsertionNode (IIndicator y) x' z' | (x,y,z) <- parts, isInsertionWord(y), x' <- (expand x n), z' <- (expand z n)] 


subtractFrom :: String -> String -> [String] 
subtractFrom xs ys = let n = (find_in xs ys 0 0) in if n == -1 then [] else [remove_from ys n (length xs)]

remove_from ys 0 0 = ys
remove_from (y:ys) 0 m = remove_from ys 0 (m-1)
remove_from (y:ys) n m = y:(remove_from ys (n-1) m)

find_in [] ys n f = n
find_in xs [] n f = -1
find_in (x:xs) (y:ys) n 0 = if x==y 
              then find_in xs ys n 1 
              else find_in (x:xs) (ys) (n+1) 0
find_in (x:xs) (y:ys) n 1 = if x==y 
              then find_in xs ys n 1
              else -1

isSubtractionWord ["without"] = True
isSubtractionWord _ = False

-- REVERSALS
makeReversalNodes :: [String] -> Int -> [ClueTree]
makeReversalNodes xs n  = let parts = twoParts xs
                  in [ReversalNode (RIndicator x) y2 | (x,y) <- includeReversals(parts), isRIndicator(x), y2 <- (expand y n)]  

isRIndicator ["returned"] = True
isRIndicator ["about"] = True
isRIndicator _ = False



-- HIDDEN WORDS
makeHiddenWordNodes :: [String]  -> Int -> [ClueTree]
makeHiddenWordNodes xs n = let parts = twoParts xs
                  in [HiddenWordNode (HWIndicator x) y | (x,y) <- parts, isHWIndicator(x)] --, (length (concat y)) > n 

isHWIndicator ["found","in"] = True
isHWIndicator ["needed","by"] = True
isHWIndicator ["from"] = True
isHWIndicator _ = False

substr [] = [[]]
substr (x:xs) = (map ((:) x) (contiguoussubstr xs)) ++ substr xs 

contiguoussubstr [] = [[]]
contiguoussubstr (x:xs) = [[x]] ++ (map ((:) x) (contiguoussubstr xs))



-- FIRST LETTERS
makeFirstLetterNodes :: [String]  -> Int -> [ClueTree]
makeFirstLetterNodes xs n = let parts = twoParts xs
                  in [FirstLetterNode (FLIndicator x) y | (x,y) <- includeReversals(parts), isFLIndicator(x), (length y) <= n]

firstLetter = map head

isFLIndicator ["leader"] = True
isFLIndicator ["at", "first"] = True
isFLIndicator ["first"] = True
isFLIndicator ["head"] = True
isFLIndicator ["first", "of"] = True
isFLIndicator _ = False




-- LAST LETTERS
makeLastLetterNodes :: [String]  -> Int -> [ClueTree]
makeLastLetterNodes xs n = let parts = twoParts xs
                  in [LastLetterNode (LLIndicator x) y | (x,y) <- includeReversals(parts), isLLIndicator(x), (length y) <= n]

lastLetter = concat . (map tail)

isLLIndicator ["in", "the ", "end"] = True
isLLIndicator ["first", "of"] = True
isLLIndicator _ = False



-- LAST LETTERS
makePartialNodes :: [String]  -> Int -> [ClueTree]
makePartialNodes xs n = let parts = twoParts xs
                  in [PartialNode (PartialIndicator x) y' | (x,y) <- includeReversals(parts), isPartialIndicator(x), y' <- (expand y n)]

top_tail_substrings :: String -> [String]
top_tail_substrings x =  top_substrings x ++ tail_substrings x

top_substrings :: String -> [String]
top_substrings (x:[]) = []  
top_substrings (x:xs) = [[x]] ++ (map  (\y -> [x] ++ y) (top_substrings xs))


tail_substrings :: String -> [String]
tail_substrings = (map reverse) . top_substrings . reverse

isPartialIndicator ["mostly"] = True
isPartialIndicator ["almost"] = True
isPartialIndicator _ = False



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

check_eval :: Parse -> [Answer]
-- check_eval x = let DefNode y z n = x in Data.List.intersect (syn y) ((eval_tree n z))
check_eval (DefNode y z n) = map (\x -> Answer x (DefNode y z n)) (Data.Set.toList (Data.Set.intersection wordlist_extended (Data.Set.fromList (eval_tree n z))))


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

-- Now we evaluate
eval :: Parse -> [Answer]
eval (DefNode y z n) = let constraints = (n, n) in [Answer x (DefNode y z n) | x <- eval_tree n z] 

eval_tree :: Int -> ClueTree  -> [String]
eval_tree n (AnagramNode x y) = if length(concat(y)) > n then [] else anagrams(concat(y))
eval_tree n (Leaf x) = filter (\x -> length x <= n) (syn x ++ [x])
eval_tree n (ConsListNode xs) = eval_trees n xs --map concat (sequence (map (eval_tree n) xs))
eval_tree n (ConsNode x y) = [x' ++ y' | x' <- eval_tree n x, y' <- eval_tree (n - length x') y]
eval_tree n (InsertionNode ind x y) = concat[insertInto x' y' | x' <- eval_tree n x, y' <- eval_tree (n - (length x')) y]
eval_tree n (SubtractionNode ind x y) = concat[subtractFrom x' y' | x' <- eval_tree n x, y' <- eval_tree n y]
eval_tree n (HiddenWordNode ind ys) = [x | x <- substr (concat ys), (length x) > 0, (length x) <= n]
eval_tree n (ReversalNode ind ys) = map reverse (eval_tree n ys)
eval_tree n (FirstLetterNode ind ys) = [firstLetter ys]
eval_tree n (LastLetterNode ind ys) = [lastLetter ys]
eval_tree n (PartialNode ind y) = concat [top_tail_substrings y | y <- eval_tree n y]
eval_tree n (ConsIndicatorLeaf x) = [""]

eval_trees :: Int -> [ClueTree] -> [String]
eval_trees n (c:[]) = eval_tree n c
eval_trees n (c:clues_left) =
  let starts = eval_tree n c
  in concatMap f starts 
  where f start =  map (\x -> start ++ x) (eval_trees (n - (length start)) clues_left)


evaluate :: [Parse] -> [Answer]
evaluate = concat . (map eval) 

sort_most_likely = (map (snd) . sort . map (\x -> (cost_parse x, x)))

possible_words = (check_valid_words . constrain_lengths  . evaluate . parse)

solve = (head . check_synonyms . check_valid_words . constrain_lengths  . evaluate . sort_most_likely . parse . lowercase) 

solve_clue = (solve . clue)

--------------------------- DICTIONARY CORNER ----------------------------

isInWordlist x = Data.Set.member x wordlist_extended
wordlist_extended = Data.Set.union (Data.Set.fromList ["swanlake", "angela", "tuckerbag", "put food in this"]) wordlist

manual_syn "notice" = ["ack", "acknowledge", "sign"] 
manual_syn "coat" = ["jacket"]
manual_syn "companion" = ["friend", "escort", "mate"]
manual_syn "shredded" = ["changed", "stripped"]
manual_syn "corset" = ["basque"]
manual_syn "flying" = ["jet"] 
manual_syn "new" = ["n"] 
manual_syn "member" = ["leg"] 
manual_syn "woman" = ["angela"] 
manual_syn "pause" = ["hesitate"] 
manual_syn "ballet" = ["swanlake"] 
manual_syn "flyer" = ["airman"] 
manual_syn "stuff" = ["tuck"]
manual_syn "put food in this" = ["tuckerbag"]
manual_syn "home counties" = ["se"]
manual_syn "school" = ["groom"]
manual_syn "good" = ["g"]
manual_syn "small worker" = ["ant"]
manual_syn "hospital department" = ["ent"]
manual_syn "fondness" = ["endearment"]
manual_syn _ = []


syn :: String -> [String]
syn ('t':'o':' ':xs) = syn xs
syn x = thes x ++ abbreviation x ++ manual_syn x

thes x = case (Map.lookup x thesaurus) of 
  Nothing -> []
  Just x -> x

abbreviation "river" = ["r"]
abbreviation "one" = ["i"]
abbreviation "very" = ["v"]
abbreviation "caught" = ["c"]
abbreviation "nationalist" = ["n"]
abbreviation _ = []

-- ghc: internal error: scavenge_stack: weird activation record found on stack: 2004205701
 
clue :: Int -> Clue
clue 1 = Clue ("companion shredded corset",6)
clue 2 = Clue ("notice in flying coat", 6)
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

  


  -- REGEX ((\d*)\s(.+)\s\((\d*)\)\n(.*)\n(.*))\n