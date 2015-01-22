module Main where 

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
import Utils
import Indicators
import Evaluation
import Dictionary
import Display
import LengthFunctions
import Abbreviation

import ClueBank
import Guardian
import Everyman



main = do 
        print $  solveClue 11
        print $  isWordlistPrefix "x"

-- Timeout is in microseconds
seconds = 1000000
dosolve x = timeout (20*seconds) $ do
            print $ solve x


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

parseClue :: [String] -> Int -> [ParseTree]
parseClue ys n= (if length ys > 1 then parseConcatNodes ys n else [])
	++ (parseWithoutConcat ys n)

parseWithoutConcat :: [String] -> Int -> [ParseTree]
parseWithoutConcat ys n = parseSynonymNodes ys n
  ++ (if length ys == 1 then parseConsIndicatorNodes ys n else [])
  ++ (if length ys > 1 then parseAnagramNodes ys n else [] )
  ++ (if length ys > 1 then parseHiddenWordNodes ys n else [])
  ++ (if length ys > 2 then parseInsertionNodes ys n else [])
  ++ (if length ys > 2 then parseSubtractionNodes ys n else [])
  ++ (if length ys > 1 then parseReversalNodes ys n else [])
  ++ (if length ys > 1 then parseFirstLetterNodes ys n else [])
  ++ (if length ys > 1 then parseLastLetterNodes ys n else [])
  ++ (if length ys > 1 then parsePartialNodes ys n else [])

parseSynonymNodes :: [String] -> Int -> [ParseTree]
parseSynonymNodes xs n = if ((length . syn . unwords $ xs) > 0) then [SynonymNode (unwords xs)] else []

parseConsNodes :: [String] -> Int -> [ParseTree]
parseConsNodes xs n = let parts = twoParts xs
                   in concat [[ConsNode x' y' |x' <- (parseClue (fst part) n), y' <- (parseClue (snd part) n)] | part <- parts]  
parseConcatNodes :: [String] -> Int -> [ParseTree]
parseConcatNodes xs n = [ConcatNode ys | ys <- (concat [sequence [parseWithoutConcat subpart n| subpart <- part] | part <- partitions xs, (length part) > 1])] --, (sum . (map minLength) $ ys) >= n, (sum . (map maxLength) $ ys) <= n ] --, (sum . map minLength) xs >= n]

parseConsIndicatorNodes :: [String] -> Int -> [ParseTree]
parseConsIndicatorNodes xs n = if isConsIndicator xs then [ConsIndicatorNode xs] else []

parseAnagramNodes :: [String] -> Int -> [ParseTree]
parseAnagramNodes xs n = let parts = twoParts xs
                  in [AnagramNode (AIndicator x) y | (x,y) <- includeReversals(parts), isAnagramWord(x), (length . concat) y <= n] 

parseInsertionNodes :: [String] -> Int -> [ParseTree]
parseInsertionNodes xs n = let parts = threeParts xs
                  in [InsertionNode (IIndicator y) x' z' | (x,y,z) <- parts, isInsertionWord(y), x' <- (parseClue x n), z' <- (parseClue z n)] 
                  ++ [InsertionNode (IIndicator y) z' x' | (x,y,z) <- parts, isReverseInsertionWord(y), x' <- (parseClue x n), z' <- (parseClue z n)] 

parseSubtractionNodes :: [String] -> Int -> [ParseTree]
parseSubtractionNodes xs n = let parts = threeParts xs
                  in [SubtractionNode (SIndicator y) x' z' | (x,y,z) <- parts, isSubtractionWord(y), x' <- (parseClue x n), z' <- (parseClue z n)] 
                  ++ [SubtractionNode (SIndicator y) x' z' | (z,y,x) <- parts, isSubtractionWord(y), x' <- (parseClue x n), z' <- (parseClue z n)] 

parseReversalNodes :: [String] -> Int -> [ParseTree]
parseReversalNodes xs n  = let parts = twoParts xs
                  in [ReversalNode (RIndicator x) y2 | (x,y) <- includeReversals(parts), isRIndicator(x), y2 <- (parseClue y n)]  

parseHiddenWordNodes :: [String]  -> Int -> [ParseTree]
parseHiddenWordNodes xs n = let parts = twoParts xs
                  in [HiddenWordNode (HWIndicator x) y | (x,y) <- parts, isHWIndicator(x)] --, (length (concat y)) > n 

parseFirstLetterNodes :: [String]  -> Int -> [ParseTree]
parseFirstLetterNodes xs n = let parts = twoParts xs
                  in [FirstLetterNode (FLIndicator x) y | (x,y) <- includeReversals(parts), isFLIndicator(x)] -- (length y) <= n

parseLastLetterNodes :: [String]  -> Int -> [ParseTree]
parseLastLetterNodes xs n = let parts = twoParts xs
                  in [LastLetterNode (LLIndicator x) y | (x,y) <- includeReversals(parts), isLLIndicator(x)] -- (length y) <= n

parsePartialNodes :: [String]  -> Int -> [ParseTree]
parsePartialNodes xs n = let parts = twoParts xs
                  in [PartialNode (PartialIndicator x) y' | (x,y) <- includeReversals(parts), isPartialIndicator(x), y' <- (parseClue y n)]

-------------- COST EVALUATION -------------

costParse (DefNode s tree n) = cost tree * (lengthPenalty s)
lengthPenalty ws = 60 + (length (words ws))   -- Magic constant here ):

cost :: ParseTree -> Int
cost (ConcatNode trees) = 20 * (length trees) + sum (map cost trees) 
cost (AnagramNode ind strings) = 10
cost (HiddenWordNode ind strings) = 40
cost (InsertionNode ind tree1 tree2) = 10 + cost tree1 + cost tree2  -- weight against complex insertions?
cost (SubtractionNode ind tree1 tree2) = 30 + cost tree1 + cost tree2
cost (ReversalNode ind tree) = 10 + cost tree
cost (SynonymNode string) = 80 * length (words string)
cost (FirstLetterNode ind strings) = 20
cost (LastLetterNode ind strings) = 20
cost (ConsNode one two) = 150
cost (PartialNode ind tree) = 60 + cost tree
cost (ConsIndicatorNode xs) = 0



--------------------- KNOWN LETTER CONSTRAINS ---------------------

knownLetterFits :: String -> String -> Bool
knownLetterFits [] [] = True
knownLetterFits [] (y:ys) = False
knownLetterFits (x:xs) [] = False
knownLetterFits (x:xs) (y:ys) = if x=='?' then (knownLetterFits xs ys) else 
                        if x==y then (knownLetterFits xs ys) else
                          False

answerFits ::  String -> Answer -> Bool
answerFits fitstring (Answer x y)  = knownLetterFits fitstring x

stripFits :: String -> [Answer] -> [Answer]
stripFits s = filter (answerFits s) 

answerPart :: Answer -> String
answerPart (Answer x y) = x

checkAnswerEquals :: String -> [Answer] -> [Answer]
checkAnswerEquals y z = filter (\x -> answerPart x == y) z

--------------------------- EVALUATION ----------------------------

isNotACheat :: Parse -> Bool
isNotACheat (DefNode def (SynonymNode ys) n) = length (syn def) >= 1
isNotACheat _ = True

removeCheats = filter isNotACheat

checkValidWords ::  [Answer] -> [Answer]
checkValidWords = filter checkValidWord

checkValidWord :: Answer -> Bool
checkValidWord (Answer x (DefNode y z n)) = isInWordlist x 

constrainParseLengths :: [Parse] -> [Parse]
constrainParseLengths = filter validParseLength

validParseLength :: Parse -> Bool
validParseLength (DefNode def clue n) = (minLength clue <= n) && (maxLength clue >= n) 


constrainLengths :: [Answer] -> [Answer]
constrainLengths = filter constrainLength

constrainLength :: Answer -> Bool
constrainLength (Answer string (DefNode def clue n))  = length (string) == n

checkSynonymns :: [Answer] -> [Answer]
checkSynonymns = filter checkSynonymn

checkSynonymn :: Answer -> Bool
checkSynonymn (Answer string (DefNode def clue n)) = Data.Set.member string (Data.Set.fromList (syn def))  

costSolved x = if checkSynonymn x then 0 else costParse (getParse x)

sortSolved = map (snd) . sort . map (\x -> (costSolved x, x))

sortMostLikely = map (snd) . sort . map (\x -> (costParse x, x))

possibleWords = checkValidWords . constrainLengths  . evaluate . parse




solve = head' . checkSynonymns . checkValidWords . constrainLengths .  evaluate . removeCheats . sortMostLikely . constrainParseLengths . parse . lowercase

solve' = solveNoSynSorted

solveNoSynSorted = head' . sortSolved  . take 100 . solveNoSyn

solveNoSynUnsorted = head'  . solveNoSyn

solveNoSyn = checkValidWords . constrainLengths  . evaluate . removeCheats . sortMostLikely . constrainParseLengths . parse . lowercase

solveClue = solve . clue

head' :: [a] -> [a]
head' []     = []
head' (x:xs) = [x]

compareClue (Clue (s,n)) (Clue (t,m)) = compare n m 

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