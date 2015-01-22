module Display where

import Types
import Utils

showDef :: Parse -> String
showDef (DefNode d tree n) = "Definition: " ++ show d ++ " \n" ++ showTree tree 1 ++ " \n\n" 

showTree :: ParseTree -> Int -> String
showTree (ConsNode x y) n = spaces n ++ showTreeL x (n+1) ++ showTreeL y (n+1)
showTree (ConcatNode xs) n = spaces n ++ "Cons \n" ++ concat (map (\x -> (showTreeL x (n+1))) xs)
showTree (AnagramNode (AIndicator anagrinds) strings) n = spaces n ++ "Anagram (" ++ unwords anagrinds ++ ") " ++ concat strings
showTree (InsertionNode (IIndicator ind) t1 t2) n = spaces n ++ "Insert ("++ unwords ind++") \n" ++ showTreeL t1 (n+1) ++ spaces n ++ "into" ++ " \n" ++ showTreeL t2 (n+1)
showTree (SubtractionNode (SIndicator ind) t1 t2) n = spaces n ++ "Subtract ("++ unwords ind++") \n" ++ showTreeL t1 (n+1) ++ spaces n ++ "from" ++ " \n" ++ showTreeL t2 (n+1)
showTree x n = spaces n ++ show x 

spaces 0 = ""
spaces n = "    " ++ spaces (n-1)

showTreeL x n = showTree x n ++ "\n"

printThis def = (putStr . showDef) def

printAll defs = mapM printThis defs

answerString (Answer x y) = x
justAnswers = map answerString