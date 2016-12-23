module Solver
  ( solve
  ) where

import Types
import Parser
import Evaluator
import Constraints

solve = chooseAnswer . evaluate . parseClue . clue 

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
