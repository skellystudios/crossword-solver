module Solver where 

import Data.List

data Clue = DefNode String ClueTree Integer
  deriving Show

data ClueTree = ConsNode ClueTree ClueTree | Leaf String | AnagramNode Anagrind [String] | InsertionNode InsertionIndicator ClueTree ClueTree | HiddenWordNode HWIndicator [String]
  deriving Show

data Anagrind = AIndicator [String] deriving Show
data InsertionIndicator = IIndicator [String] deriving Show
data HWIndicator = HWIndicator [String] deriving Show


-- Memoiszation

data Tree a = Tree (Tree a) a (Tree a)
instance Functor Tree where
    fmap f (Tree l m r) = Tree (fmap f l) (f m) (fmap f r)


index :: Tree a -> Int -> a
index (Tree _ m _) 0 = m
index (Tree l _ r) n = case (n - 1) `divMod` 2 of
    (q,0) -> index l q
    (q,1) -> index r q

{-
nats :: Tree Int
nats = go 0 1
    where
        go !n !s = Tree (go l s') n (go r s')
            where
                l = n + s
                r = l + s
                s' = s * 2
toList :: Tree a -> [a]
toList as = map (index as) [0..]


f_tree :: Tree Int
f_tree = fmap (f fastest_f) nats

fastest_f :: Int -> Int
fastest_f = index f_tree

-}

--- DISPLAY FUNCTIONS

showDef :: Clue -> String
showDef (DefNode d tree n) = "Definition: " ++ show d ++ " \n" ++ showTree tree 1 ++ " \n" 

showTree :: ClueTree -> Int -> String
showTree (ConsNode x y) n = spaces n ++ showTree x (n+1) ++ " \n" ++ showTree y (n+1)
showTree (AnagramNode (AIndicator anagrinds) strings) n = spaces n ++ "Anagram (" ++ concatWithSpaces anagrinds ++ ") " ++ concat strings
showTree x n = spaces n ++ show x


spaces 0 = ""
spaces n = "    " ++ spaces (n-1)


{- 
  map putStr (map showDef (makeDefs clue3))
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


makeDefs :: ([String], Integer) -> [Clue]
makeDefs (xs, n) = let parts = twoParts xs
			  in concat [[DefNode (concatWithSpaces (fst part)) y' n| y' <- (expand (snd part))] | part <- includeReversals (parts)]

expand :: [String] -> [ClueTree]
expand ys = [Leaf (concatWithSpaces ys)] 
	++ (if length ys > 1 then makeAnagramNodes ys else [] )
	++ (if length ys > 1 then makeConsNodes ys else [])
	++ (if length ys > 1 then makeHiddenWordNodes ys else [])
	++ (if length ys > 2 then makeInsertionNodes ys else [])




makeConsNodes :: [String] -> [ClueTree]
makeConsNodes xs = let parts = twoParts xs
                   in concat [[ConsNode x' y' |x' <- (expand (fst part)), y' <- (expand (snd part))] | part <- parts]  

-- ANAGRAMS
 
-- Sometimes need to use synonymns here ??? Maybe anagram subtypes needs to be a special type of subtree
makeAnagramNodes :: [String] -> [ClueTree]
makeAnagramNodes xs = let parts = twoParts xs
                  in [AnagramNode (AIndicator x) y | (x,y) <- parts, isAnagramWord(x)] 

isAnagramWord :: [String] -> Bool
isAnagramWord ["mixed"] = True
isAnagramWord ["shredded"] = True
isAnagramWord ["flying"] = True
isAnagramWord _ = False

anagrams :: String -> [String]
anagrams x = anagrams1 x []

anagrams1 :: String -> String -> [String]
anagrams1 [] ys = [ys]
--anagrams1 (x:[]) ys = [x:ys, ys ++ [x]]
anagrams1 (x:xs) ys = (anagrams1 xs (x:ys)) ++ (anagrams1 xs (ys++[x]))


-- INSERTIONS
makeInsertionNodes :: [String] -> [ClueTree]
makeInsertionNodes xs = let parts = threeParts xs
                  in [InsertionNode (IIndicator y) x' z' | (x,y,z) <- parts, isInsertionWord(y), x' <- (expand x), z' <- (expand z)] 


insertInto :: String -> String -> [String] -- TODO: we don't want to have insertInto 'abc' 'xyz' = abcxyz
insertInto xs [] = [xs]
insertInto xs (y:ys) = [y:(xs ++ ys)] ++ (map ((:) y) (insertInto xs ys)) 


isInsertionWord ["in"] = True
isInsertionWord _ = False


-- HIDDEN WORDS
makeHiddenWordNodes :: [String] -> [ClueTree]
makeHiddenWordNodes xs = let parts = twoParts xs
                  in [HiddenWordNode (HWIndicator x) y | (x,y) <- parts, isHWIndicator(x)] 

isHWIndicator ["found","in"] = True
isHWIndicator _ = False

substr [] = [[]]
substr (x:xs) = (map ((:) x) (contiguoussubstr xs)) ++ substr xs 

contiguoussubstr [] = [[]]
contiguoussubstr (x:xs) = [[x]] ++ (map ((:) x) (contiguoussubstr xs))



--------------------------- EVALUATION ----------------------------

-- Now we evaluate
eval :: Clue -> [String]
eval x = let DefNode y z n = x in Data.List.intersect (syn y) (eval_tree z)

eval_tree :: ClueTree -> [String]
eval_tree (AnagramNode x y) = anagrams(concat(y))
eval_tree (Leaf x) = syn x ++ [x]
eval_tree (ConsNode x y) = [x' ++ y' | x' <- eval_tree(x), y' <- eval_tree(y)]
eval_tree (InsertionNode ind x y) = concat[insertInto x' y' | x' <- eval_tree(x), y' <- eval_tree(y)]
eval_tree (HiddenWordNode ind ys) = substr (concat ys)

syn :: String -> [String]

{-}
syn "notice" = ["ack", "acknowledge", "sign"] 
syn "coat" = ["jacket"]
syn "companion" = ["friend", "escort", "mate"]
syn "shredded" = ["changed", "stripped"]
syn "corset" = ["basque"]
syn "flying" = ["jet"] 
syn _ = [] -}

syn "notice" = ["ack", "account","acquaintance","admonition","advertisement","advice","advisory","alarm","analysis","announcement","annunciation","appreciation","assiduity","attend","attention","awareness","behold","blackmail","blue book","briefing","bulletin","call for","call","care","caution","caveat","censure","circular","claim","cognizance","comment","commentary","communication","communique","conceive","concentration","consciousness","consideration","contribution","credible","criticism","critique","data","datum","declaration","descry","detect","diligence","directive","directory","discern","discharge","discover","dismissal","dispatch","distinguish","dope","draft","drain","draught","draughtsman","draughty","duty","ear","earnestness","edict","editorial","encyclical","enlightenment","enunciation","espial","espionage","espy","evidence","exaction","example","extortion","facts","feel","find","glimpse","gloss","goods","guidebook","handout","hark","heed","hint","identify","imposition","impost","indent","info","injunction","insight","inspect","instruction","intelligence","intentness","interdict","interest","item","ken","know","knowledge","leader","lesson","levy","light","look on","look","lookout","make out","mandate","manifesto","mark","memo","memorandum","mention","message","mind","monition","moral","note","notice","notice","account","acknowledge","acquaintance","admonish","admonishment","admonition","advert","advertence","advertency","advice","advise","alarm","alertness","allude","analysis","animadvert","announce","announcement","annunciation","apperception","appreciation","appreciativeness","apprehension","approval","assiduity","assiduousness","attend","attend to","attention","attention span","attentiveness","awareness","behold","bench warrant","blackmail","blue book","book review","briefing","bulletin","bulletin board","call","call for","capias","care","catch sight of","caution","caveat","censure","circular","claim","clap eyes on","cognition","cognizance","comment","commentary","commentation","communication","communique","concentration","concern","consciousness","consideration","contribution","critical bibliography","critical journal","critical notice","critical review","criticism","critique","data","datum","death warrant","declaration","demand","demand for","descry","detect","deterrent example","diligence","directive","directory","discern","discover","dispatch","distinguish","draft","drain","duty","ear","earnestness","edict","editorial","encyclical","enlightenment","enunciation","espial","espionage","espy","evidence","exaction","example","extortion","extortionate demand","facts","factual information","familiarization","fieri facias","final notice","final warning","gen","general information","give heed to","give notice","glimpse","gloss","grasp","guidebook","habere facias possessionem","handout","hard information","have in sight","heavy demand","heed","heedfulness","hint","identify","imposition","impost","incidental information","indent","info","inform","information","injunction","insight","insistent demand","instruction","intelligence","intentiveness","intentness","interdict","intimation","ken","knowledge","lay eyes on","leader","leading article","lesson","levy","light","literary criticism","look","look on","look upon","looking","lookout","make out","mandamus","mandate","mandatory injunction","manifesto","mark","memo","mention","message","mind","mindfulness","mittimus","monition","moral","nisi prius","noesis","nonnegotiable demand","note","notification","notify","object lesson","observance","observation","observe","order","pay attention to","perceive","perception","pick out","pick up","position paper","precept","presentation","press release","process","proclamation","program","programma","prohibitory injunction","promotional material","pronouncement","pronunciamento","proof","public notice","publication","publicity","realization","recognition","recognize","refer","regard","regardfulness","release","remark","report","requirement","requisition","respect","review","running commentary","rush","rush order","search warrant","see","sense","sensibility","sidelight","sight","spot","spy","spying","statement","take heed of","take in","take note","take note of","take notice","take notice of","tax","taxing","tend","the dope","the goods","the know","the scoop","thought","threat","tip-off","transmission","tribute","twig","ukase","ultimatum","understanding","verbum sapienti","view","viewing","warn","warning","warning piece","warrant","warrant of arrest","warrant of attorney","watch","watching","white book","white paper","witness","witnessing","word","writ","write-up","notification","object lesson","observance","observation","observe","order","pamphlet","perceive","perception","pick out","pick up","pipe","position paper","poster","precept","presentation","process","proclamation","program","pronouncement","proof","publication","publicity","puff","push","realization","recognition","recognize","regard","release","remark","reminder","report","requirement","requisition","resignation","respect","review","reviewer","rush","savor","scoop","seal","search warrant","see","sense","sensibility","sidelight","sight","sign","spot","spy","spying","statement","take in","take notice","tax","taxing","tend","thought","threat","tip off","transmission","tribute","twig","ukase","ultimatum","view","warning","warrant","watch","white paper","witness","word to the wise","word","writ","write up"]
syn "coat" = ["bedaub","bedizen","begild","besmear","blanket","boot","bristle","buff","butter","calcimine","cap","cloak","coat","coat","Eton jacket","Leatherette","Leatheroid","Mao jacket","anorak","apply paint","bedaub","bedizen","begild","besmear","blanket","blazer","blouse","body coat","bolero","bomber jacket","bonnet","boot","breech","bristle","brush on paint","butter","calcimine","cap","capillament","capuchin","car coat","chaqueta","chesterfield","chromogen","cilium","claw hammer","claw-hammer coat","cloak","coat of paint","coating","coif","collop","color","color filter","color gelatin","colorant","coloring","complexion","cover","coverage","covering","covert","coverture","cowl","cowling","curtain","cut","cutaway coat","cuticle","dab","daub","dead-color","deal","deep-dye","dermis","dinner jacket","dip","disk","distemper","double-dye","doublet","drape","drapery","dress coat","drier","duffel","dye","dyestuff","emblazon","enamel","engild","exterior paint","face","facing","fast-dye","fell","feuille","film","fingertip coat","fitted coat","flap","flat coat","flat wash","fleece","flesh","floor enamel","foil","fold","fresco","frock","frock coat","fur","furring","gild","glaze","gloss","gown","grain","greatcoat","ground","guise","hair","hanging","hat","hide","hood","horsehair","housing","hue","illuminate","imbue","imitation fur","imitation leather","ingrain","integument","interior paint","jacket","japan","jerkin","jumper","jupe","lacquer","lamella","lamina","laminated glass","laminated wood","lap","lay on","lay on color","layer","leaf","leather","leather paper","loden coat","mackinaw","mane","mantle","mask","medium","membrane","mess jacket","midicoat","monkey jacket","opaque color","outer layer","outer skin","overcoat","overlay","paint","pall","pane","panel","parget","parka","patina","pea jacket","peel","pellicle","pelt","peltry","pigment","pile","plait","plank","plate","plating","ply","plywood","prime","prime coat","primer","priming","pubescence","pubic hair","rasher","rawhide","reefer","revetment","rind","sack","safety glass","san benito","scale","screen","scum","setula","shade","shadow","shag","sheath","sheet","shellac","shelter","shield","shirt","shoe","shroud","ski jacket","skin","skins","slab","slap on","slat","slather","sleeve waistcoat","slice","slop on paint","smear","smear on","smoking jacket","sock","spiketail coat","spread","spread on","spread with","stain","stipple","stocking","swallowtail","tabard","table","tablet","tail coat","tails","tar","tegument","tempera","thinner","tinct","tinction","tincture","tinge","tint","tone","topcoat","transparent color","turpentine","turps","undercoat","undercoating","vair","varnish","vehicle","veil","veneer","vestment","wafer","wash","wash coat","watch coat","whitewash","windbreaker","wool","woolly","coating","coif","collop","color","coloring","cover","covering","covert","cowl","curtain","cut","cuticle","dab","daub","deal","dermis","dip","disk","distemper","drape","drapery","drier","dye","emblazon","emblem","enamel","face","facing","fell","film","flap","fleece","flesh","foil","fold","fresco","fur","gild","glaze","gloss","gown","grain","ground","guise","hair","hanging","hat","hide","hood","horsehair","housing","hue","illuminate","imbue","ingrain","integument","jacket","lacquer","lap","lay it on thick","lay on","leaf","leather","mane","mantle","mask","medium","membrane","overcoat","paint","pall","pane","panel","parget","patina","peel","pellicle","pelt","pigment","pile","plait","plank","plaster","plate","plating","ply","plywood","powder","prime","primer","priming","rasher","rawhide","rind","safety glass","scale","screen","scum","seta","shade","shadow","shag","sheath","sheet","shellac","shelter","shield","shoe","shroud","skin","slab","slap on","slat","slather","slice","smear","sock","spread","stain","stipple","table","tablet","tar","tempera","thinner","tincture","tinge","tint","tone","turpentine","undercoat","varnish","vehicle","veil","veneer","vestment","wafer","wash","whitewash","wool"] 
syn "companion" = ["friend", "escort", "mate"]
syn "shredded" = ["chalky","cleft","cloven","cracked","crushed","cut","dusty","farinaceous","fine","flaky","furfuraceous","grated","ground","impalpable","lacerated","mangled","mealy","milled","mutilated","powdered","powdery","pulverized","quartered","ragged","rent","scaly","scurfy","severed","shredded","shredded","branny","chalklike","chalky","cleft","cloven","comminute","comminuted","cracked","crushed","cut","detrital","detrited","disintegrated","dusty","efflorescent","farinaceous","fine","flaky","floury","furfuraceous","gone to dust","grated","ground","impalpable","in pieces","in shreds","lacerate","lacerated","levigated","mangled","mealy","milled","mutilated","pestled","powdered","powdery","pulverant","pulverized","pulverulent","quartered","ragged","reduced to powder","rent","riven","scaly","scobicular","scobiform","scurfy","severed","sharded","slit","splintered","split","tattered","torn","triturated","slit","split","tattered","torn"]
syn "corset" = ["advocate","arm","back","backbone","backing","bandeau","bearer","bra","brace","bracer","bracket","brassiere","buttress","cane","carrier","cervix","corset","corset","advocate","alpenstock","arm","athletic supporter","back","backbone","backing","bandeau","bearer","bra","brace","bracer","bracket","brassiere","buttress","cane","carrier","cervix","corselet","crook","crutch","foundation garment","fulcrum","girdle","guy","guywire","jock","jockstrap","mainstay","maintainer","mast","neck","prop","reinforce","reinforcement","reinforcer","rest","resting place","rigging","shoulder","shroud","spine","sprit","staff","standing rigging","stave","stay","stays","stick","stiffener","strengthener","support","supporter","sustainer","upholder","walking stick","crook","crutch","foundation garment","fulcrum","girdle","guy","guywire","jock","mainstay","mast","neck","petticoat","prop","reinforcement","rest","resting place","rigging","shoulder","shroud","spine","staff","stave","stay","stick","strengthener","support","supporter","sustainer","undergarments","upholder","walking stick"]
syn "flying" = ["jet", "aeronautics","agile","airline","ascending","astronautics","axial","back","backward","ballooning","breakneck","brittle","capricious","changeable","corruptible","cruising","cursory","dashing","deciduous","descending","dissolving","double quick","downward","drifting","dying","ecstatic","ephemeral","evanescent","expeditious","express","fading","fast","feverish","fickle","fleet","fleeting","flight","flitting","flowing","fluent","fluttering","fly-by-night","flying","flying","aeronautics","agile","air service","airborne","airline","ascending","astronautics","aviation","axial","back","back-flowing","backward","ballooning","blind flying","breakneck","brittle","capricious","changeable","cloud-seeding","commercial aviation","contact flying","corruptible","cruising","cursory","dashing","deciduous","descending","disappearing","dissolving","double-quick","down-trending","downward","drifting","dying","eagle-winged","ephemeral","evanescent","evaporating","expeditious","express","fading","fast","festinate","feverish","fickle","fleet","fleeting","flight","flitting","flowing","fluent","fluttering","fly-by-night","fragile","frail","fugacious","fugitive","furious","galloping","general aviation","gliding","going","gyrational","gyratory","hair-trigger","hasty","headlong","hovering","hurried","hustling","immediate","impermanent","impetuous","impulsive","inconstant","instant","insubstantial","jet-propelled","last-minute","light of heel","light-footed","lively","melting","mercurial","momentary","mortal","mounting","mutable","nimble","nimble-footed","nondurable","nonpermanent","on the spot","passing","perishable","pilotage","plunging","precipitate","progressive","prompt","quick","quick as lightning","quick as thought","rapid","reckless","reflowing","refluent","regressive","retrogressive","rising","rocket-propelled","rotary","rotational","rotatory","running","rushing","sailing","sailplaning","short-lived","sideward","sinking","slap-bang","slapdash","snap","snappy","soaring","spanking","speedy","streaming","superficial","swift","temporal","temporary","transient","transitive","transitory","undurable","unenduring","unstable","up-trending","upward","urgent","vanishing","volant","volatile","volitant","winged","winging","fragile","frail","fugacious","fugitive","furious","galloping","going","gyrational","hair trigger","hasty","headlong","hurried","immediate","impermanent","impetuous","impulsive","inconstant","instant","insubstantial","last minute","light footed","lively","melting","mercurial","momentary","mortal","mounting","mutable","nimble footed","nimble","nonpermanent","on the spot","passing","perishable","pilotage","plunging","precipitate","progressive","prompt","quick","rapid","reckless","refluent","regressive","retrogressive","rising","rotary","running","rushing","sailing","short lived","sideward","sinking","slapdash","snap","snappy","soaring","spanking","speedy","streaming","superficial","swift","temporal","temporary","transient","transitive","transitory","undurable","unstable","upward","urgent","vanishing","volatile","winged"]
  
syn _ = []

ignore_blanks xs = [x | x <- xs, not (x==[])]
solve = ignore_blanks . (map eval) . makeDefs
 
clue1 = (words "companion shredded corset",7)
clue2 = (words "notice in flying coat", 6)
clue3 = (words "companion found in oklahoma terminal", 4)



-- Cons node equivalence - write it as a list, don't allow cons node as a child

