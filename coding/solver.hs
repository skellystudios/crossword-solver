module Solver where 

import Data.List 
import qualified Data.Set
import Wordlist
--import Data.String.Utils
 

data Clue = DefNode String ClueTree Int
  deriving Show

data ClueTree = ConsListNode [ClueTree] | ConsNode ClueTree ClueTree | Leaf String | AnagramNode Anagrind [String] | InsertionNode InsertionIndicator ClueTree ClueTree | SubtractionNode SubtractionIndicator ClueTree ClueTree | HiddenWordNode HWIndicator [String] | ReversalNode ReversalIndicator ClueTree
  deriving Show

data Answer = Answer String Clue deriving Show

data Anagrind = AIndicator [String] deriving Show
data InsertionIndicator = IIndicator [String] deriving Show
data SubtractionIndicator = SIndicator [String] deriving Show
data ReversalIndicator = RIndicator [String] deriving Show
data HWIndicator = HWIndicator [String] deriving Show


--- TOOD SECTION
-- TODO: Preprocessing to weight to most likely: first (map c) will be lazy
-- TODO: Pass constraints down the eval chain, in the same way we pass them down the parse chain
-- TODO: A function that makes a printed clue markup version (Clue -> String) [ah, but hard, as we don't create a total parse tree including subs etc, at the end]

-- TODO: do a thing wherein we deal with the problem with leaf nodes not evaluating to anything. THIS IS WHERE I CAN USE A MAYBE A MONAD
-- TODO: Sometimes need to use synonymns when doing anagrams ??? Maybe anagram subtypes needs to be a special type of subtree
-- TODO: we don't want to have insertInto 'abc' 'xyz' = abcxyz
-- TODO: Change subtraction eval function from insert, obvs
-- TODO: Add some abbreviation function
-- TODO: Reversal clues
-- TODO: First/last letter clues
-- TODO: Before/after clues

--- DISPLAY FUNCTIONS

showDef :: Clue -> String
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


parse :: (String, Int) -> [Clue]
parse (xs, n) = makeNoIndicatorDefs (words xs, n) ++ makIndicatorDefs (words xs, n)

makeNoIndicatorDefs :: ([String], Int) -> [Clue]
makeNoIndicatorDefs (xs, n) = let parts = twoParts xs
        in concat [[DefNode (concatWithSpaces (fst part)) y' n| y' <- (expand (snd part) n), Data.Set.member (concatWithSpaces (fst part)) wordlist2] | part <- includeReversals (parts)]

makIndicatorDefs :: ([String], Int) -> [Clue]
makIndicatorDefs (xs, n) = let parts = threeParts xs
        in concat [[DefNode (concatWithSpaces x) z' n| z' <- (expand z n)] | (x,y,z) <- (parts), isDefIndicator(y), Data.Set.member (concatWithSpaces x) wordlist2] 
        ++ concat [[DefNode (concatWithSpaces x) z' n| z' <- (expand z n)] | (z,y,x) <- (parts), isDefIndicator(y), Data.Set.member (concatWithSpaces x) wordlist2]


isDefIndicator ["in"] = True
isDefIndicator ["is"] = True
isDefIndicator _ = False

expand :: [String] -> Int -> [ClueTree]
expand ys n= [Leaf (concatWithSpaces ys)] 
	++ (if length ys > 1 then makeAnagramNodes ys n else [] )
	++ (if length ys > 1 then makeConsListNodes ys n else [])
	++ (if length ys > 1 then makeHiddenWordNodes ys n else [])
  ++ (if length ys > 2 then makeInsertionNodes ys n else [])
  ++ (if length ys > 1 then makeReversalNodes ys n else [])

expandNoCons :: [String] -> Int -> [ClueTree]
expandNoCons ys n = [Leaf (concatWithSpaces ys)] 
  ++ (if length ys > 1 then makeAnagramNodes ys n else [] )
  ++ (if length ys > 1 then makeHiddenWordNodes ys n else [])
  ++ (if length ys > 2 then makeInsertionNodes ys n else [])
  ++ (if length ys > 1 then makeReversalNodes ys n else [])

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


maxLength (ConsListNode trees) = (sum . map maxLength) trees
maxLength (AnagramNode ind strings) = (length . concat) strings
maxLength (HiddenWordNode ind strings) = (length strings) - 2
maxLength (InsertionNode ind tree1 tree2) = (minLength tree1) + (minLength tree2)
maxLength (SubtractionNode ind tree1 tree2) = minimum[(maxLength tree1) - (minLength tree2),3]
maxLength (ReversalNode ind tree) = maxLength tree
maxLength (Leaf string) = let x = maximum ( map length (string : syn string)) in x


---------------- CLUE TYPES ----------------

makeConsNodes :: [String] -> Int -> [ClueTree]
makeConsNodes xs n = let parts = twoParts xs
                   in concat [[ConsNode x' y' |x' <- (expand (fst part) n), y' <- (expand (snd part) n)] | part <- parts]  


makeConsListNodes :: [String] -> Int -> [ClueTree]
makeConsListNodes xs n = [ConsListNode xs | xs <- (concat [sequence [expandNoCons subpart n| subpart <- part] | part <- partitions xs, (length part) > 1]), (sum . map minLength) xs >= n]


-- SUCH THAT sum(map (minLength) xs) <= clueLength and sum(map (maxLength) xs) >= clue

-- ANAGRAMS

makeAnagramNodes :: [String] -> Int -> [ClueTree]
makeAnagramNodes xs n = let parts = twoParts xs
                  in [AnagramNode (AIndicator x) y | (x,y) <- includeReversals(parts), isAnagramWord(x)] 

isAnagramWord :: [String] -> Bool
isAnagramWord ["mixed"] = True
isAnagramWord ["shredded"] = True
isAnagramWord ["flying"] = True
isAnagramWord ["twisted"] = True
isAnagramWord ["fancy"] = True
isAnagramWord ["at sea"] = True
isAnagramWord _ = False

anagrams :: String -> [String]
anagrams [] = [[]]
anagrams xs = [x:ys | x<-xs, ys <- anagrams(delete x xs)]

-- INSERTIONS
makeInsertionNodes :: [String] -> Int -> [ClueTree]
makeInsertionNodes xs n = let parts = threeParts xs
                  in [InsertionNode (IIndicator y) x' z' | (x,y,z) <- parts, isInsertionWord(y), x' <- (expand x n), z' <- (expand z n)] 


insertInto :: String -> String -> [String] 
insertInto xs [] = [xs]
insertInto xs (y:ys) = [y:(xs ++ ys)] ++ (map ((:) y) (insertInto xs ys)) 

isInsertionWord ["in"] = True
isInsertionWord _ = False


-- SUBTRACTIONS
makeSubtractionNodes :: [String] -> Int -> [ClueTree]
makeSubtractionNodes xs n = let parts = threeParts xs
                  in [InsertionNode (IIndicator y) x' z' | (x,y,z) <- parts, isInsertionWord(y), x' <- (expand x n), z' <- (expand z n)] 


subtractFrom :: String -> String -> [String] 
subtractFrom xs [] = [xs]
subtractFrom xs (y:ys) = [y:(xs ++ ys)] ++ (map ((:) y) (subtractFrom xs ys)) 

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
                  in [HiddenWordNode (HWIndicator x) y | (x,y) <- parts, isHWIndicator(x)] 

isHWIndicator ["found","in"] = True
isHWIndicator ["needed","by"] = True
isHWIndicator _ = False

substr [] = [[]]
substr (x:xs) = (map ((:) x) (contiguoussubstr xs)) ++ substr xs 

contiguoussubstr [] = [[]]
contiguoussubstr (x:xs) = [[x]] ++ (map ((:) x) (contiguoussubstr xs))



--------------------------- EVALUATION ----------------------------



check_eval :: Clue -> [Answer]
-- check_eval x = let DefNode y z n = x in Data.List.intersect (syn y) ((eval_tree n z))
check_eval (DefNode y z n) = map (\x -> Answer x (DefNode y z n)) (Data.Set.toList (Data.Set.intersection wordlist2 (Data.Set.fromList (eval_tree n z))))

check_valid_words :: Answer -> Bool
check_valid_words _ = True

constrain_lengths :: [Answer] -> [Answer]
constrain_lengths = filter constrain_length

constrain_length :: Answer -> Bool
constrain_length (Answer string (DefNode def clue n))  = length (string) == n

check_synonyms :: [Answer] -> [Answer]
check_synonyms = filter check_synonym

check_synonym :: Answer -> Bool
check_synonym (Answer string (DefNode def clue n)) = Data.Set.member string (Data.Set.fromList (syn def))  

-- Now we evaluate
eval :: Clue -> [Answer]
eval (DefNode y z n) = [Answer x (DefNode y z n) | x <- eval_tree n z]

eval_tree :: Int -> ClueTree  -> [String]
eval_tree n (AnagramNode x y) = if length(concat(y)) > n then [] else anagrams(concat(y))
eval_tree n (Leaf x) = syn x ++ [x]
eval_tree n (ConsListNode xs) = map concat (sequence (map (eval_tree n) xs))
eval_tree n (ConsNode x y) = [x' ++ y' | x' <- eval_tree n x, y' <- eval_tree n y]
eval_tree n (InsertionNode ind x y) = concat[insertInto x' y' | x' <- eval_tree n x, y' <- eval_tree n y]
eval_tree n (SubtractionNode ind x y) = concat[subtractFrom x' y' | x' <- eval_tree n x, y' <- eval_tree n y]
eval_tree n (HiddenWordNode ind ys) = [x | x <- substr (concat ys), (length x) > 0]
eval_tree n (ReversalNode ind ys) = map reverse (eval_tree n ys)



find_solutions :: [Clue] -> [(Clue, [String])]
find_solutions xs = map (\x -> (x, eval x)) xs


-- solve = ignore_blanks . (map eval) . parse
solve c =  map (check_eval) (parse c)

solve2 = (check_synonyms . constrain_lengths . concat. solve . clue)

--------------------------- DICTIONARY CORNER ----------------------------


wordlist2 = Data.Set.union (Data.Set.fromList ["swanlake", "angela", "tuckerbag", "put food in this"]) wordlist


syn :: String -> [String]

 {-

syn "notice" = ["ack", "acknowledge", "sign"] 
syn "coat" = ["jacket"]
syn "companion" = ["friend", "escort", "mate"]
syn "shredded" = ["changed", "stripped"]
syn "corset" = ["basque"]
syn "flying" = ["jet"] 
syn "new" = ["n"] 
syn "member" = ["leg"] 
syn "woman" = ["angela"] 
syn "pause" = ["hesitate"] 
syn "ballet" = ["swanlake"] 
syn "flyer" = ["airman"] 
syn "stuff" = ["tuck"]
syn "put food in this" = ["tuckerbag"]


-- -}

-- {-

syn "notice" = ["ack", "account","acquaintance","admonition","advertisement","advice","advisory","alarm","analysis","announcement","annunciation","appreciation","assiduity","attend","attention","awareness","behold","blackmail","blue book","briefing","bulletin","call for","call","care","caution","caveat","censure","circular","claim","cognizance","comment","commentary","communication","communique","conceive","concentration","consciousness","consideration","contribution","credible","criticism","critique","data","datum","declaration","descry","detect","diligence","directive","directory","discern","discharge","discover","dismissal","dispatch","distinguish","dope","draft","drain","draught","draughtsman","draughty","duty","ear","earnestness","edict","editorial","encyclical","enlightenment","enunciation","espial","espionage","espy","evidence","exaction","example","extortion","facts","feel","find","glimpse","gloss","goods","guidebook","handout","hark","heed","hint","identify","imposition","impost","indent","info","injunction","insight","inspect","instruction","intelligence","intentness","interdict","interest","item","ken","know","knowledge","leader","lesson","levy","light","look on","look","lookout","make out","mandate","manifesto","mark","memo","memorandum","mention","message","mind","monition","moral","note","notice","notice","account","acknowledge","acquaintance","admonish","admonishment","admonition","advert","advertence","advertency","advice","advise","alarm","alertness","allude","analysis","animadvert","announce","announcement","annunciation","apperception","appreciation","appreciativeness","apprehension","approval","assiduity","assiduousness","attend","attend to","attention","attention span","attentiveness","awareness","behold","bench warrant","blackmail","blue book","book review","briefing","bulletin","bulletin board","call","call for","capias","care","catch sight of","caution","caveat","censure","circular","claim","clap eyes on","cognition","cognizance","comment","commentary","commentation","communication","communique","concentration","concern","consciousness","consideration","contribution","critical bibliography","critical journal","critical notice","critical review","criticism","critique","data","datum","death warrant","declaration","demand","demand for","descry","detect","deterrent example","diligence","directive","directory","discern","discover","dispatch","distinguish","draft","drain","duty","ear","earnestness","edict","editorial","encyclical","enlightenment","enunciation","espial","espionage","espy","evidence","exaction","example","extortion","extortionate demand","facts","factual information","familiarization","fieri facias","final notice","final warning","gen","general information","give heed to","give notice","glimpse","gloss","grasp","guidebook","habere facias possessionem","handout","hard information","have in sight","heavy demand","heed","heedfulness","hint","identify","imposition","impost","incidental information","indent","info","inform","information","injunction","insight","insistent demand","instruction","intelligence","intentiveness","intentness","interdict","intimation","ken","knowledge","lay eyes on","leader","leading article","lesson","levy","light","literary criticism","look","look on","look upon","looking","lookout","make out","mandamus","mandate","mandatory injunction","manifesto","mark","memo","mention","message","mind","mindfulness","mittimus","monition","moral","nisi prius","noesis","nonnegotiable demand","note","notification","notify","object lesson","observance","observation","observe","order","pay attention to","perceive","perception","pick out","pick up","position paper","precept","presentation","press release","process","proclamation","program","programma","prohibitory injunction","promotional material","pronouncement","pronunciamento","proof","public notice","publication","publicity","realization","recognition","recognize","refer","regard","regardfulness","release","remark","report","requirement","requisition","respect","review","running commentary","rush","rush order","search warrant","see","sense","sensibility","sidelight","sight","spot","spy","spying","statement","take heed of","take in","take note","take note of","take notice","take notice of","tax","taxing","tend","the dope","the goods","the know","the scoop","thought","threat","tip-off","transmission","tribute","twig","ukase","ultimatum","understanding","verbum sapienti","view","viewing","warn","warning","warning piece","warrant","warrant of arrest","warrant of attorney","watch","watching","white book","white paper","witness","witnessing","word","writ","write-up","notification","object lesson","observance","observation","observe","order","pamphlet","perceive","perception","pick out","pick up","pipe","position paper","poster","precept","presentation","process","proclamation","program","pronouncement","proof","publication","publicity","puff","push","realization","recognition","recognize","regard","release","remark","reminder","report","requirement","requisition","resignation","respect","review","reviewer","rush","savor","scoop","seal","search warrant","see","sense","sensibility","sidelight","sight","sign","spot","spy","spying","statement","take in","take notice","tax","taxing","tend","thought","threat","tip off","transmission","tribute","twig","ukase","ultimatum","view","warning","warrant","watch","white paper","witness","word to the wise","word","writ","write up"]
syn "coat" = ["bedaub","bedizen","begild","besmear","blanket","boot","bristle","buff","butter","calcimine","cap","cloak","coat","coat","Eton jacket","Leatherette","Leatheroid","Mao jacket","anorak","apply paint","bedaub","bedizen","begild","besmear","blanket","blazer","blouse","body coat","bolero","bomber jacket","bonnet","boot","breech","bristle","brush on paint","butter","calcimine","cap","capillament","capuchin","car coat","chaqueta","chesterfield","chromogen","cilium","claw hammer","claw-hammer coat","cloak","coat of paint","coating","coif","collop","color","color filter","color gelatin","colorant","coloring","complexion","cover","coverage","covering","covert","coverture","cowl","cowling","curtain","cut","cutaway coat","cuticle","dab","daub","dead-color","deal","deep-dye","dermis","dinner jacket","dip","disk","distemper","double-dye","doublet","drape","drapery","dress coat","drier","duffel","dye","dyestuff","emblazon","enamel","engild","exterior paint","face","facing","fast-dye","fell","feuille","film","fingertip coat","fitted coat","flap","flat coat","flat wash","fleece","flesh","floor enamel","foil","fold","fresco","frock","frock coat","fur","furring","gild","glaze","gloss","gown","grain","greatcoat","ground","guise","hair","hanging","hat","hide","hood","horsehair","housing","hue","illuminate","imbue","imitation fur","imitation leather","ingrain","integument","interior paint","jacket","japan","jerkin","jumper","jupe","lacquer","lamella","lamina","laminated glass","laminated wood","lap","lay on","lay on color","layer","leaf","leather","leather paper","loden coat","mackinaw","mane","mantle","mask","medium","membrane","mess jacket","midicoat","monkey jacket","opaque color","outer layer","outer skin","overcoat","overlay","paint","pall","pane","panel","parget","parka","patina","pea jacket","peel","pellicle","pelt","peltry","pigment","pile","plait","plank","plate","plating","ply","plywood","prime","prime coat","primer","priming","pubescence","pubic hair","rasher","rawhide","reefer","revetment","rind","sack","safety glass","san benito","scale","screen","scum","setula","shade","shadow","shag","sheath","sheet","shellac","shelter","shield","shirt","shoe","shroud","ski jacket","skin","skins","slab","slap on","slat","slather","sleeve waistcoat","slice","slop on paint","smear","smear on","smoking jacket","sock","spiketail coat","spread","spread on","spread with","stain","stipple","stocking","swallowtail","tabard","table","tablet","tail coat","tails","tar","tegument","tempera","thinner","tinct","tinction","tincture","tinge","tint","tone","topcoat","transparent color","turpentine","turps","undercoat","undercoating","vair","varnish","vehicle","veil","veneer","vestment","wafer","wash","wash coat","watch coat","whitewash","windbreaker","wool","woolly","coating","coif","collop","color","coloring","cover","covering","covert","cowl","curtain","cut","cuticle","dab","daub","deal","dermis","dip","disk","distemper","drape","drapery","drier","dye","emblazon","emblem","enamel","face","facing","fell","film","flap","fleece","flesh","foil","fold","fresco","fur","gild","glaze","gloss","gown","grain","ground","guise","hair","hanging","hat","hide","hood","horsehair","housing","hue","illuminate","imbue","ingrain","integument","jacket","lacquer","lap","lay it on thick","lay on","leaf","leather","mane","mantle","mask","medium","membrane","overcoat","paint","pall","pane","panel","parget","patina","peel","pellicle","pelt","pigment","pile","plait","plank","plaster","plate","plating","ply","plywood","powder","prime","primer","priming","rasher","rawhide","rind","safety glass","scale","screen","scum","seta","shade","shadow","shag","sheath","sheet","shellac","shelter","shield","shoe","shroud","skin","slab","slap on","slat","slather","slice","smear","sock","spread","stain","stipple","table","tablet","tar","tempera","thinner","tincture","tinge","tint","tone","turpentine","undercoat","varnish","vehicle","veil","veneer","vestment","wafer","wash","whitewash","wool"] 
syn "companion" = ["friend", "escort", "mate"]
syn "shredded" = ["chalky","cleft","cloven","cracked","crushed","cut","dusty","farinaceous","fine","flaky","furfuraceous","grated","ground","impalpable","lacerated","mangled","mealy","milled","mutilated","powdered","powdery","pulverized","quartered","ragged","rent","scaly","scurfy","severed","shredded","shredded","branny","chalklike","chalky","cleft","cloven","comminute","comminuted","cracked","crushed","cut","detrital","detrited","disintegrated","dusty","efflorescent","farinaceous","fine","flaky","floury","furfuraceous","gone to dust","grated","ground","impalpable","in pieces","in shreds","lacerate","lacerated","levigated","mangled","mealy","milled","mutilated","pestled","powdered","powdery","pulverant","pulverized","pulverulent","quartered","ragged","reduced to powder","rent","riven","scaly","scobicular","scobiform","scurfy","severed","sharded","slit","splintered","split","tattered","torn","triturated","slit","split","tattered","torn"]
syn "corset" = ["advocate","arm","back","backbone","backing","bandeau","bearer","bra","brace","bracer","bracket","brassiere","buttress","cane","carrier","cervix","corset","corset","advocate","alpenstock","arm","athletic supporter","back","backbone","backing","bandeau","bearer","bra","brace","bracer","bracket","brassiere","buttress","cane","carrier","cervix","corselet","crook","crutch","foundation garment","fulcrum","girdle","guy","guywire","jock","jockstrap","mainstay","maintainer","mast","neck","prop","reinforce","reinforcement","reinforcer","rest","resting place","rigging","shoulder","shroud","spine","sprit","staff","standing rigging","stave","stay","stays","stick","stiffener","strengthener","support","supporter","sustainer","upholder","walking stick","crook","crutch","foundation garment","fulcrum","girdle","guy","guywire","jock","mainstay","mast","neck","petticoat","prop","reinforcement","rest","resting place","rigging","shoulder","shroud","spine","staff","stave","stay","stick","strengthener","support","supporter","sustainer","undergarments","upholder","walking stick"]
syn "flying" = ["jet", "aeronautics","agile","airline","ascending","astronautics","axial","back","backward","ballooning","breakneck","brittle","capricious","changeable","corruptible","cruising","cursory","dashing","deciduous","descending","dissolving","double quick","downward","drifting","dying","ecstatic","ephemeral","evanescent","expeditious","express","fading","fast","feverish","fickle","fleet","fleeting","flight","flitting","flowing","fluent","fluttering","fly-by-night","flying","flying","aeronautics","agile","air service","airborne","airline","ascending","astronautics","aviation","axial","back","back-flowing","backward","ballooning","blind flying","breakneck","brittle","capricious","changeable","cloud-seeding","commercial aviation","contact flying","corruptible","cruising","cursory","dashing","deciduous","descending","disappearing","dissolving","double-quick","down-trending","downward","drifting","dying","eagle-winged","ephemeral","evanescent","evaporating","expeditious","express","fading","fast","festinate","feverish","fickle","fleet","fleeting","flight","flitting","flowing","fluent","fluttering","fly-by-night","fragile","frail","fugacious","fugitive","furious","galloping","general aviation","gliding","going","gyrational","gyratory","hair-trigger","hasty","headlong","hovering","hurried","hustling","immediate","impermanent","impetuous","impulsive","inconstant","instant","insubstantial","jet-propelled","last-minute","light of heel","light-footed","lively","melting","mercurial","momentary","mortal","mounting","mutable","nimble","nimble-footed","nondurable","nonpermanent","on the spot","passing","perishable","pilotage","plunging","precipitate","progressive","prompt","quick","quick as lightning","quick as thought","rapid","reckless","reflowing","refluent","regressive","retrogressive","rising","rocket-propelled","rotary","rotational","rotatory","running","rushing","sailing","sailplaning","short-lived","sideward","sinking","slap-bang","slapdash","snap","snappy","soaring","spanking","speedy","streaming","superficial","swift","temporal","temporary","transient","transitive","transitory","undurable","unenduring","unstable","up-trending","upward","urgent","vanishing","volant","volatile","volitant","winged","winging","fragile","frail","fugacious","fugitive","furious","galloping","going","gyrational","hair trigger","hasty","headlong","hurried","immediate","impermanent","impetuous","impulsive","inconstant","instant","insubstantial","last minute","light footed","lively","melting","mercurial","momentary","mortal","mounting","mutable","nimble footed","nimble","nonpermanent","on the spot","passing","perishable","pilotage","plunging","precipitate","progressive","prompt","quick","rapid","reckless","refluent","regressive","retrogressive","rising","rotary","running","rushing","sailing","short lived","sideward","sinking","slapdash","snap","snappy","soaring","spanking","speedy","streaming","superficial","swift","temporal","temporary","transient","transitive","transitory","undurable","unstable","upward","urgent","vanishing","volatile","winged"]



---}

syn _ = []

 
clue :: Int -> (String, Int)
clue 1 = ("companion shredded corset",6)
clue 2 = ("notice in flying coat", 6)
clue 3 = ("companion found in oklahoma terminal", 4)
clue 4 = ("a new member returned a woman", 6)
clue 5 = ("pause at these i fancy", 8) -- Everyman 3526, clue 1   ["athetise","hesitate"] 
clue 6 = ("ankle was twisted in ballet", 8) -- Everyman 3526, clue 3
clue 7 = ("flyer needed by funfair manager", 6)
clue 8 = ("put food in this stuff on barge at sea", 9)
-- 3.4 GB memory used

