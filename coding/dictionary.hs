module Dictionary where

import Data.Set
import Data.Map as Map

import Wordlists

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
manual_syn "about" = ["c"]
manual_syn "a thousand" = ["k", "m"]
manual_syn "church" = ["c"]
manual_syn "wine" = ["aseti"]
manual_syn "providing" = ["if"]
manual_syn "theme" = ["leitmotif"]
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