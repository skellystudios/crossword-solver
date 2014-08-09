
--- TOOD SECTION

-- Need to radically improve wordlist
-- Need to include "Small" as abbreviation indicator

-- Come up with a name 
-- Come up with a cryptic clue for the thesis title: as Jack Devlin for help!

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
