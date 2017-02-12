module EvaluationCosts
  where

import Types

evaluationCost :: ParsedClue -> Int
evaluationCost pc@(ParsedClue ((Clue (_, len)), def, indicator, tree))
  = (parseTreeCost tree) * (lengthPenalty def)

lengthPenalty :: Definition -> Int
lengthPenalty ws = 60 + (length (words ws))   -- Magic constant here ):

parseTreeCost :: ParseTree -> Int
parseTreeCost (ConcatC trees) = 20 * (length trees) + sum (map parseTreeCost trees)
parseTreeCost (JuxtC i tree1 tree2) =  10 + parseTreeCost tree1 + parseTreeCost tree2
parseTreeCost (AnagC i ws) = 10
parseTreeCost (HiddenC ind strings) = 40
parseTreeCost (InsertC ind tree1 tree2) = 30 + parseTreeCost tree1 + parseTreeCost tree2  -- weight against complex insertions?
parseTreeCost (SubC ind tree1 tree2) = 30 + parseTreeCost tree1 + parseTreeCost tree2
parseTreeCost (RevC ind tree) = 10 + parseTreeCost tree
parseTreeCost (SynC phrase) = 80 * length (words phrase)
parseTreeCost (FirstsC ind strings) = 20
parseTreeCost (LastsC ind strings) = 20
parseTreeCost (PartC ind tree) = 60 + parseTreeCost tree
parseTreeCost (BeforeC ind tree1 tree2) = 10 + parseTreeCost tree1 + parseTreeCost tree2  -- weight against complex insertions?
parseTreeCost (AfterC ind tree1 tree2) = 10 + parseTreeCost tree1 + parseTreeCost tree2  -- weight against complex insertions?
parseTreeCost (IdentC tree1) = 5
