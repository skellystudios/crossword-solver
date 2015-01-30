module Dictionary where

import Data.Set
import Data.Map as Map

import Wordlists
import Abbreviation
import Types

isInWordlist x = Data.Set.member x wordlistExtended'
wordlistExtended' = Data.Set.union (Data.Set.fromList (Map.keys thesaurus)) wordlistExtended 
wordlistExtended = Data.Set.union (Data.Set.fromList ["swanlake", "angela", "tuckerbag", "put food in this", "earnest request"]) wordlist

isPrefix = isWordlistPrefix
isWordlistPrefix x = Data.Set.member x wordlistPrefix
wordlistPrefix = Data.Set.fold addPrefixes Data.Set.empty wordlistExtended'	
addPrefixes word set = Data.Set.union (Data.Set.fromList (prefixes word)) set

prefixes = rprefixes . reverse 
rprefixes (x:xs) = [reverse xs++[x]] ++ rprefixes xs
rprefixes [] = []
 

manualSynonym "notice" = ["ack", "acknowledge", "sign"] 
manualSynonym "coat" = ["jacket"]
manualSynonym "companion" = ["friend", "escort", "mate"]
manualSynonym "shredded" = ["changed", "stripped"]
manualSynonym "corset" = ["basque"]
manualSynonym "flying" = ["jet"] 
manualSynonym "new" = ["n"] 
manualSynonym "member" = ["leg"] 
manualSynonym "woman" = ["angela"] 
manualSynonym "pause" = ["hesitate"] 
manualSynonym "ballet" = ["swanlake"] 
manualSynonym "flyer" = ["airman"] 
manualSynonym "stuff" = ["tuck"]
manualSynonym "put food in this" = ["tuckerbag"]
manualSynonym "home counties" = ["se"]
manualSynonym "school" = ["groom"]
manualSynonym "good" = ["g"]
manualSynonym "small worker" = ["ant"]
manualSynonym "hospital department" = ["ent"]
manualSynonym "fondness" = ["endearment"]
manualSynonym "about" = ["c"]
manualSynonym "a thousand" = ["k", "m"]
manualSynonym "church" = ["c"]
manualSynonym "wine" = ["aseti"]
manualSynonym "providing" = ["if"]
manualSynonym "theme" = ["leitmotif"]
manualSynonym "not public" = ["secret"]
manualSynonym "earnest request" = ["prayer"]
manualSynonym "paper in the street" = ["litter"]
manualSynonym "article" = ["a", "an"]
manualSynonym "girlfriend" = ["gf"]
manualSynonym _ = []


isSynonymn x y = elem x (syn y)

syn :: String -> [String]
syn ('t':'o':' ':xs) = syn xs
syn x = Prelude.filter (not . Prelude.null) $ thes x ++ abbreviation x ++ manualSynonym x ++ abbreviation' x



thes x = decodeThesaurus x





abbreviation' "spades" = ["s"]
abbreviation' "river" = ["r"]
abbreviation' "one" = ["i"]
abbreviation' "very" = ["v"]
abbreviation' "caught" = ["c"]
abbreviation' "nationalist" = ["n"]
abbreviation' _ = []