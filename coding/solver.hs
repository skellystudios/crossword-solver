module Solver where 

import Data.List  
import qualified Data.Set
import qualified Data.Map as Map
import System.Environment   
  
import Wordlist
import Thesaurus
import Anagram



--import Data.String.Utils
 



data Parse = DefNode String ClueTree Int
  deriving (Show, Eq, Ord)

data ClueTree = ConsIndicatorLeaf [String] | ConsListNode [ClueTree] | ConsNode ClueTree ClueTree | Leaf String | AnagramNode Anagrind [String] | InsertionNode InsertionIndicator ClueTree ClueTree | SubtractionNode SubtractionIndicator ClueTree ClueTree | HiddenWordNode HWIndicator [String] | ReversalNode ReversalIndicator ClueTree | FirstLetterNode FLIndicator [String] | LastLetterNode LLIndicator [String] | PartialNode PartialIndicator ClueTree
   deriving (Show, Eq, Ord)

data Answer = Answer String Parse deriving (Show, Eq, Ord)

data Anagrind = AIndicator [String] deriving (Show, Eq, Ord)
data InsertionIndicator = IIndicator [String] deriving (Show, Eq, Ord)
data SubtractionIndicator = SIndicator [String] deriving (Show, Eq, Ord)
data ReversalIndicator = RIndicator [String] deriving (Show, Eq, Ord)
data HWIndicator = HWIndicator [String] deriving (Show, Eq, Ord)
data FLIndicator = FLIndicator [String] deriving (Show, Eq, Ord)
data LLIndicator = LLIndicator [String] deriving (Show, Eq, Ord)
data PartialIndicator = PartialIndicator [String] deriving (Show, Eq, Ord)

data Constrains = MaxLength MinLength
data MaxLength = D Int
data MinLength = Int


--- TOOD SECTION

-- TODO: Pre-process anagrams and pass them through

-- TODO: Preprocessing to weight to most likely: first (map c) will be lazy

-- TODO: A function that makes a printed clue markup version (Clue -> String) [ah, but hard, as we don't create a total parse tree including subs etc, at the end]
-- TODO: Conditional eval on insertion node should be smarter 
-- TODO: Do some sort of statistical evaluation to determine the cost function. Or, like, machine learn it?

-- TODO: do a thing wherein we deal with the problem with leaf nodes not evaluating to anything. THIS IS WHERE I CAN USE A MAYBE A MONAD
-- TODO: we don't want to have insertInto 'abc' 'xyz' = abcxyz
-- TODO: Change subtraction eval function from insert, obvs
-- TODO: Add some abbreviation function
-- TODO: Sometimes need to use synonymns when doing anagrams ??? Maybe anagram subtypes needs to be a special type of subtree

-- WRITE UP / RESEARCH: 

-- TODO: Write something about reverses and trees for output - and implications for type system.
-- TODO: Garbage Collection, write about it.
-- TODO: Concurrency - make it fun!
-- TODO: Look up Suffix trees to compress thesaurus

--- DISPLAY FUNCTIONS




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



{- 
  map putStr (map showDef (parse clue3))
-}


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


parse :: (String, Int) -> [Parse]
parse (xs, n) = makeNoIndicatorDefs (words xs, n) ++ makIndicatorDefs (words xs, n)

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
--  ++ (if length ys > 2 then makeConsIndicatorNodes ys n else [])
  ++ (if length ys > 1 then makeReversalNodes ys n else [])
  ++ (if length ys > 1 then makeFirstLetterNodes ys n else [])
  ++ (if length ys > 1 then makeLastLetterNodes ys n else [])
  ++ (if length ys > 1 then makePartialNodes ys n else [])

expandJustAbbreviations :: [String] -> Int -> [ClueTree]
expandJustAbbreviations ys n = [Leaf (concatWithSpaces ys)] 


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
cost (InsertionNode ind tree1 tree2) = 40 + cost tree1 + cost tree2  -- TODO: weight against complex insertions?
cost (SubtractionNode ind tree1 tree2) = 30 + cost tree1 + cost tree2
cost (ReversalNode ind tree) = 20 + cost tree
cost (Leaf string) = 80 * length (words string)
cost (FirstLetterNode ind strings) = 20
cost (LastLetterNode ind strings) = 20
cost (ConsNode one two) = 150
cost (PartialNode ind tree) = 60 + cost tree
cost (ConsIndicatorLeaf xs) = 0



---------------- CLUE TYPES ----------------

makeConsNodes :: [String] -> Int -> [ClueTree]
makeConsNodes xs n = let parts = twoParts xs
                   in concat [[ConsNode x' y' |x' <- (expand (fst part) n), y' <- (expand (snd part) n)] | part <- parts]  


makeConsListNodes :: [String] -> Int -> [ClueTree]
makeConsListNodes xs n = [ConsListNode xs | xs <- (concat [sequence [expandNoCons subpart n| subpart <- part] | part <- partitions xs, (length part) > 1])] --, (sum . map minLength) xs >= n]

-- Make cons indicator and then filter them out afterwards

makeConsIndicatorNodes :: [String] -> Int -> [ClueTree]
makeConsIndicatorNodes xs n = if isConsIndicator xs then [ConsIndicatorLeaf xs] else []

-- makeConsIndicatorNodes xs n = let parts = threeParts xs
--                    in concat [[ConsNode x' y' |(x, ind, y) <- parts, x' <- (expand x n), y' <- (expand y n), isConsIndicator(ind)] | part <- parts]  


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
isReverseInsertionWord ["around"] = True
isReverseInsertionWord _ = False

-- SUBTRACTIONS
makeSubtractionNodes :: [String] -> Int -> [ClueTree]
makeSubtractionNodes xs n = let parts = threeParts xs
                  in [InsertionNode (IIndicator y) x' z' | (x,y,z) <- parts, isInsertionWord(y), x' <- (expand x n), z' <- (expand z n)] 


subtractFrom :: String -> String -> [String] 
subtractFrom xs [] = [xs]
subtractFrom xs (y:ys) = [y:(xs ++ ys)] ++ (map ((:) y) (subtractFrom xs ys)) 


-- remove (x:xs) (y:ys) = if (x==y) then 


--replace old new = intercalate new . Data.List.Split.splitOn old

{-}

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new l = joins new . splitOn old $ l


joins :: [a] -> [[a]] -> [a]
joins delim l = concat (intersperse delim l)

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
  | f x = splitOn f xs
  | otherwise = let (h,t) = break f l in h:(splitOn f t)

-}

isSubtractionWord ["without"] = True
isSubtractionWord _ = False

-- REVERSALS
makeReversalNodes :: [String] -> Int -> [ClueTree]
makeReversalNodes xs n  = let parts = twoParts xs
                  in [ReversalNode (RIndicator x) y2 | (x,y) <- includeReversals(parts), isRIndicator(x), y2 <- (expand y n)]  

isRIndicator ["returned"] = True
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
{-
find_solutions :: [Clue] -> [(Clue, [String])]
find_solutions xs = map (\x -> (x, eval x)) xs
--}

-- solve = ignore_blanks . (map eval) . parse
-- solve c =  map (check_eval) (parse c)

sort_most_likely = (map (snd) . sort . map (\x -> (cost_parse x, x)))


possible_words = (check_valid_words . constrain_lengths  . evaluate . parse)

solve = (head . check_synonyms . check_valid_words . constrain_lengths  . evaluate . sort_most_likely . parse)

solve_clue = (solve . clue)



--------------------------- DICTIONARY CORNER ----------------------------


isInWordlist x = Data.Set.member x wordlist_extended
wordlist_extended = Data.Set.union (Data.Set.fromList ["swanlake", "angela", "tuckerbag", "put food in this"]) wordlist


syn :: String -> [String]


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
manual_syn _ = []


-- syn _ = []
syn ('t':'o':' ':xs) = syn xs
syn x = thes x ++ abbreviation x ++ manual_syn x

thes x = case (Map.lookup x thesaurus) of 
  Nothing -> []
  Just x -> x

abbreviation "river" = ["r"]
abbreviation "one" = ["i"]
abbreviation _ = []


-- ghc: internal error: scavenge_stack: weird activation record found on stack: 2004205701
 
clue :: Int -> (String, Int)
clue 1 = ("companion shredded corset",6)
clue 2 = ("notice in flying coat", 6)
clue 3 = ("companion found in oklahoma terminal", 4)
clue 4 = ("a new member returned a woman", 6)
clue 5 = ("pause at these i fancy", 8) -- Everyman 3526, clue 1   ["athetise","hesitate"] 
clue 6 = ("ankle was twisted in ballet", 8) -- Everyman 3526, clue 3
clue 7 = ("flyer needed by funfair manager", 6)
clue 8 = ("put food in this stuff on barge at sea", 9) -- Why doesn't this work?
clue 9 = ("notice supervisor is going nuts at first", 4)
clue 10 = ("animal makes mistake crossing one river", 7)
clue 11 = ("maria not a fickle lover", 9)
clue 12 = ("hope for high praise", 6)

main = solve_clue 8


qualified