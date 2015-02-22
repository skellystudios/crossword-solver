module Indicators where

import qualified Data.Set as Set
import Utilities

isDefIndicator ws
  = elem (unwords ws) defIndicators

defIndicators
  = ["show", "give", "and", "to", "in", "to get", "to give", "brings out",
     "to make a", "for", "is", "providing", "makes", "made by", "for a"]

isReverseJuxtapositionIndicator ["opposite"] = True
isReverseJuxtapositionIndicator ["after"] = True
isReverseJuxtapositionIndicator _ = False

isJuxtapositionIndicator ["on"] = True
isJuxtapositionIndicator ["next","to"] = True
isJuxtapositionIndicator ["with"] = True
isJuxtapositionIndicator ["beside"] = True
isJuxtapositionIndicator ["above"] = True
isJuxtapositionIndicator ["to"] = True
isJuxtapositionIndicator ["over"] = True
isJuxtapositionIndicator _ = False


isInsertionIndicator :: [String] -> Bool
isInsertionIndicator xs = Set.member (concatWithSpaces xs) insertionIndicators
insertionIndicators = Set.fromList [
	"dwells in",
	"characters",
	"buried in",
	"content",
	"contributing",
	"contents",
	"extract",
	"held by",
	"furnishes",
	"appears in",
	"gives",
	"camouflages",
	"aboard",
	"lining",
	"occupying",
	"admitted",
	"amidst",
	"packing",
	"besieged",
	"boarding",
	"parting",
	"boxed",
	"breaking",
	"penetrating",
	"bridged",
	"caught",
	"circled",
	"piercing",
	"contained",
	"content",
	"contents",
	"restrained",
	"covered",
	"clutched",
	"dividing",
	"embraced",
	"ringed",
	"encircled",
	"separating",
	"enfolded",
	"entering",
	"enveloped",
	"filling",
	"flanked",
	"grasped",
	"splitting",
	"harboured",
	"held",
	"stuffing",
	"housed",
	"surrounded",
	"inside",
	"internal",
	"interrupting",
	"introduced",
	"lining",
	"occupying",
	"taken in",
	"packing",
	"parting",
	"penetrating",
	"tucked into",
	"piercing",
	"ringed",
	"within",
	"separating",
	"sheltered",
	"splitting",
	"stuffing",
	"surrounded",
	"swallowed",
	"taken in",
	"tucked in",
	"within",
	"wrapped",
	"aboard",
	"accommodated",
	"admitted",
	"besieged",
	"amidst",
	"between",
	"boarding",
	"boxed",
	"breaking",
	"buried",
	"bridged",
	"carried by",
	"caught",
	"circled",
	"contained",
	"contents",
	"covered",
	"clutched",
	"dividing",
	"embraced",
	"encircled",
	"enfolded",
	"entering",
	"enveloped",
	"filling",
	"flanked",
	"grasped",
	"harboured",
	"held",
	"housed",
	"imbibed",
	"in",
	"included in",
	"helping to make",
	"inside",
	"hidden",
	"internal",
	"hides",
	"interrupting",
	"introduced",
	"inwardly",
	"in",
	"inherent in",
	"inside",
	"internal",
	"intrinsic",
	"letters from",
	"letters of",
	"part",
	"partial",
	"partly",
	"segment of",
	"selection",
	"smuggling",
	"some",
	"somewhat",
	"within",
  "wearing",
	"involved in",
	"admitted to",
	"taken in by"]

isReverseInsertionIndicator :: [String] -> Bool
isReverseInsertionIndicator xs = Set.member (concatWithSpaces xs) reverseInsertionIndicators
reverseInsertionIndicators = Set.fromList [
	"to include",
	"on",
	"to pen",
	"frames",
	"grasping",
	"harbouring",
	"flanks",
	"envelops",
	"holds",
	"houses",
  "keeps",
	"surrounds",
	"including",
	"housing",
  "munching",
	"outside",
	"outwardly",
	"overwhelms",
	"swallows",
	"protects",
	"enfolds",
	"receives",
	"sheltering",
	"enveloping",
	"surroundings",
	"restrains",
	"rings",
	"round",
	"external",
	"about",
	"about",
	"accepts",
	"admit",
	"admits",
	"admits",
	"admitting",
	"around",
	"around",
	"besiege",
	"besieges",
	"besieges",
	"besieging",
	"box",
	"boxes",
	"boxes",
	"boxing",
	"bridge",
	"bridges",
	"bridges",
	"bridging",
	"capture",
	"captured",
	"captures",
	"captures",
	"capturing",
	"carries",
	"catch",
	"catches",
	"catches",
	"catching",
	"circle",
	"circles",
	"circling",
	"circling",
	"clutch",
	"clutches",
	"clutches",
	"clutching",
	"concealing",
	"contain",
	"containing",
	"containing",
	"contains",
	"cover",
	"covering",
	"covers",
	"covers",
	"embrace",
	"embraces",
	"embracing",
	"embracing",
	"encircle",
	"encircles",
	"encircles",
	"encircling",
	"enfold",
	"enfolding",
	"enfolds",
	"envelop",
	"enveloping",
	"envelops",
	"external",
	"flank",
	"flanking",
	"flanks",
	"frame",
	"framed",
	"frames",
	"framing",
	"grasp",
	"grasping",
	"grasps",
	"harbour",
	"harbouring",
	"harbours",
	"hold",
	"holding",
	"holds",
	"house",
	"houses",
	"housing",
	"outside",
	"ring",
	"ringing",
	"rings",
	"round",
	"shelter",
	"sheltering",
	"shelters",
	"surround",
	"surrounding",
	"surrounds",
	"swallow",
	"swallowing",
	"swallows",
	"take in",
	"takes in",
	"takes in",
	"taking in",
	"traps",
	"without",
	"wrap",
	"wrapping",
	"wraps",
	"wraps",
	"concealed",
	"concealing",
	"conceals",
	"contained",
	"demonstrates",
	"displaying",
	"eclipsing",
	"encloses",
	"exhibiting",
	"featuring",
	"has",
	"hiding",
	"holds",
	"providing",
	"secreting",
	"sheltered",
	"showing",
	"stores",
	"swallowed",
	"veiled",
	"veiling",
	"veils",
  "wearing",
	"withholds",
	"wrapped",
	"crossing",
	"contains",
	"to secure",
	"about",
	"around",
	"carrying"]



isReversalIndicator ["after","recovery"] = True
isReversalIndicator ["returned"] = True
isReversalIndicator ["returns"] = True
isReversalIndicator ["springs","back"] = True
isReversalIndicator ["about"] = True
isReversalIndicator _ = False

isHiddenWordIndicator ["found","in"] = True
isHiddenWordIndicator ["needed","by"] = True
isHiddenWordIndicator ["from"] = True
isHiddenWordIndicator ["inside"] = True
isHiddenWordIndicator _ = False

isFirstLetterIndicator ["leader"] = True
isFirstLetterIndicator ["leaders"] = True
isFirstLetterIndicator ["all","leaders"] = True
isFirstLetterIndicator ["all","leaders","in"] = True
isFirstLetterIndicator ["at","first"] = True
isFirstLetterIndicator ["first"] = True
isFirstLetterIndicator ["head"] = True
isFirstLetterIndicator ["head", "of"] = True
isFirstLetterIndicator ["first","of"] = True
isFirstLetterIndicator _ = False

isLastLetterIndicator ["in","the ","end"] = True
isLastLetterIndicator ["end","of"] = True
isLastLetterIndicator _ = False

isPartOfIndicator ["dropping","guts"] = True
isPartOfIndicator ["bit","of"] = True
isPartOfIndicator ["part","of"] = True
isPartOfIndicator ["mostly"] = True
isPartOfIndicator ["partly"] = True
isPartOfIndicator ["almost"] = True
isPartOfIndicator ["nearly"] = True
isPartOfIndicator ["headless"] = True
isPartOfIndicator ["tailless"] = True
isPartOfIndicator _ = False

isSubtractionIndicator ["having removed"] = True
isSubtractionIndicator ["has lost"] = True
isSubtractionIndicator ["dropping"] = True
isSubtractionIndicator ["drops"] = True
isSubtractionIndicator ["dropped"] = True
isSubtractionIndicator ["lost"] = True
isSubtractionIndicator ["loses"] = True
isSubtractionIndicator ["without"] = True
isSubtractionIndicator ["letting","slip"] = True
isSubtractionIndicator _ = False

isReverseSubtractionIndicator ["leaving"] = True
isReverseSubtractionIndicator ["from"] = True
isReverseSubtractionIndicator ["taken from"] = True
isReverseSubtractionIndicator ["from"] = True
isReverseSubtractionIndicator ["from"] = True
isReverseSubtractionIndicator _ = False

isAnagramIndicator :: [String] -> Bool
isAnagramIndicator xs = Set.member (concatWithSpaces xs) anagramIndicators

anagramIndicators = Set.fromList [
  "amended",
  "advantage",
  "abandoned",
  "aberration",
  "aberrations",
  "abnormal",
  "about",
  "absurd",
  "abused",
  "abysmal",
  "accident",
  "acrobatics",
  "action",
  "active",
  "adapted",
  "addled",
  "adjusted",
  "adrift",
  "affected",
  "afresh",
  "after a fashion",
  "agile",
  "agitated",
  "all over the place",
  "altered",
  "alternative",
  "amazing",
  "amend",
  "amendment",
  "amiss",
  "amok",
  "anarchic",
  "anew",
  "animated",
  "anomalous",
  "another",
  "anyhow",
  "around",
  "arrange",
  "arrangement",
  "artful",
  "askew",
  "assembled",
  "assorted",
  "astray",
  "at odds",
  "at random",
  "atrocious",
  "at sea",
  "at sixes and sevens",
  "awful",
  "awkward",
  "awry",
  "bad",
  "baffled",
  "bake",
  "bananas",
  "barking",
  "bastardization",
  "bats",
  "battered",
  "batty",
  "beaten",
  "beaten up",
  "become",
  "bedlam",
  "belted",
  "bend",
  "bendy",
  "bent",
  "berserk",
  "bespoke",
  "bizarre",
  "blasted",
  "blazing",
  "blend",
  "blown up",
  "boiled",
  "break up",
  "brewed",
  "broach",
  "broadcast",
  "broken",
  "broth",
  "buckled",
  "buffalo",
  "building",
  "built",
  "bungled",
  "burst",
  "bust",
  "bustling",
  "busy",
  "butcher",
  "by arrangement",
  "by mistake",
  "Byzantine",
  "can be",
  "careless",
  "carved up",
  "cast",
  "cavorting",
  "changed",
  "changes shape",
  "changing",
  "chaotic",
  "choppy",
  "chopped up",
  "churn",
  "circulated",
  "clobber",
  "cocktail",
  "cock-up",
  "code",
  "compilation",
  "complex",
  "complicated",
  "compose",
  "composition",
  "compound",
  "comprising",
  "concealed",
  "concoction",
  "confused",
  "constituents",
  "construction",
  "contorted",
  "contrived",
  "conversion",
  "converted",
  "convoluted",
  "convulsing",
  "cooked",
  "correct",
  "correction",
  "corrupt",
  "could be",
  "crack",
  "cracked",
  "cracked up",
  "crackers",
  "craftily",
  "crashed",
  "crazy",
  "criminal",
  "crooked",
  "crude",
  "crumbly",
  "crushed",
  "cuckoo",
  "cunningly",
  "curious",
  "daft",
  "damaged",
  "dancing",
  "dashing",
  "defective",
  "deformed",
  "delirious",
  "demented",
  "demolish",
  "deplorable",
  "deployed",
  "deranged",
  "derived from",
  "design",
  "desperate",
  "destroyed",
  "destruction",
  "desultory",
  "devastated",
  "developed",
  "deviant",
  "devilish",
  "devious",
  "dicky",
  "different",
  "disarray",
  "disfigured",
  "disguised",
  "dished out",
  "dishevelled",
  "disintegrating",
  "disjointed",
  "dislocated",
  "dismantled",
  "disorderly",
  "disorganized",
  "disorientated",
  "dispersed",
  "disposed",
  "disrupted",
  "disseminated",
  "distilled",
  "distorted",
  "distraught",
  "distressed",
  "disturbed",
  "disturbance",
  "diverse",
  "dizzy",
  "doctor",
  "doddery",
  "dodgy",
  "done",
  "dotty",
  "dreadful",
  "dressed",
  "drifting",
  "drunk",
  "dubious",
  "dud",
  "duff",
  "dynamic",
  "easily",
  "eccentried",
  "elaborate",
  "elastic",
  "embroil",
  "emendation",
  "engendering",
  "engineer",
  "ensemble",
  "entangle",
  "err",
  "errant",
  "erratic",
  "erroneous",
  "erupt",
  "eruption",
  "evolution",
  "exchange",
  "exchanged",
  "excited",
  "exercising",
  "exotic",
  "explode",
  "explosive",
  "extraordinary",
  "extravagant",
  "fabricated",
  "fake",
  "false",
  "fanatical",
  "fancy",
  "fantastic",
  "fashion",
  "faulty",
  "fermented",
  "fickle",
  "fiddled",
  "fishy",
  "fixed",
  "flailing",
  "flexibly",
  "floundering",
  "flow",
  "fluctuate",
  "fluid",
  "flurry",
  "flustered",
  "flying",
  "foolish",
  "for a change",
  "forced",
  "forged",
  "form",
  "foul",
  "fractured",
  "fragmented",
  "frantic",
  "freak",
  "freaking out",
  "freely",
  "frenetic",
  "frenzied",
  "frenzy",
  "fresh",
  "freshly",
  "frightfully",
  "frisky",
  "frolicsome",
  "fuddled",
  "fudge",
  "funny",
  "furiously",
  "gaffe",
  "gambol",
  "garbled",
  "generating",
  "gets",
  "giddily",
  "gingerly",
  "giving",
  "going crazy",
  "gone",
  "go off",
  "grim",
  "groomed",
  "grotesque",
  "ground",
  "hammered",
  "haphazard",
  "harassed",
  "harmed",
  "hash",
  "havoc",
  "haywire",
  "hectic",
  "hideous",
  "higgledy-piggledy",
  "high",
  "hit",
  "horribly",
  "horrid",
  "horrific",
  "hotchpotch",
  "hybrid",
  "idiotic",
  "ill-bred",
  "ill-disposed",
  "ill-formed",
  "ill-treated",
  "impure",
  "imperfect",
  "improper",
  "inaccurate",
  "in a ferment",
  "in a jumble",
  "in a mess",
  "in a whirl",
  "incorrect",
  "in disarray",
  "in disguise",
  "inebriated",
  "inept",
  "in error",
  "ingredients",
  "injured",
  "in motion",
  "inordinate",
  "in pieces",
  "in ruins",
  "insanely",
  "interfered with",
  "intoxicated",
  "intricate",
  "invention",
  "involved",
  "irregular",
  "irritated",
  "itinerant",
  "jaunty",
  "jazzy",
  "jerky",
  "jiggling",
  "jittery",
  "jolting",
  "jostling",
  "juddering",
  "juggle",
  "jumble",
  "jumbled",
  "jumping",
  "junk",
  "kind of",
  "kinky",
  "knead",
  "knocked about",
  "knotted",
  "laboured",
  "labyrinthine",
  "lawless",
  "lax",
  "liberally",
  "liquid",
  "lively",
  "loony",
  "loosely",
  "lost",
  "ludicrous",
  "lunatic",
  "mad",
  "made-up",
  "madness",
  "make",
  "make up",
  "malformed",
  "malfunction",
  "malleably",
  "maltreated",
  "managed",
  "mangle",
  "manic",
  "manipulate",
  "manoeuvre",
  "manufacturing",
  "mar",
  "mash",
  "mashed",
  "massage",
  "maul",
  "maybe",
  "mayhem",
  "meandering",
  "medley",
  "melange",
  "mêlée",
  "melted",
  "mental",
  "mess",
  "metamorphosis",
  "mince",
  "mint",
  "misbehaves",
  "misbehaving",
  "mischievously",
  "misfit",
  "misguided",
  "mishandled",
  "mishap",
  "misinterpret",
  "mislead",
  "mismanagement",
  "misplace",
  "misprint",
  "misrepresented",
  "misshapen",
  "misspell",
  "mistake",
  "mistreat",
  "misuse",
  "mix",
  "mixed up",
  "mixture",
  "mix-up",
  "mobile",
  "mobilize",
  "modelled",
  "modification",
  "mongrel",
  "mount",
  "move",
  "movement",
  "moving",
  "muddled",
  "muff",
  "munching",
  "mushy",
  "mutation",
  "mutilation",
  "mutinous",
  "mysterious",
  "nastily",
  "naughty",
  "neatened",
  "new",
  "not in order",
  "not properly",
  "not right",
  "novel",
  "nuts",
  "nutty",
  "obstreperous",
  "odd",
  "oddball",
  "off",
  "orderly",
  "organization",
  "organized",
  "originally",
  "ornate",
  "otherwise",
  "out",
  "outlandish",
  "out of hand",
  "out of joint",
  "out of order",
  "out of place",
  "out of sorts",
  "outrageous",
  "outré",
  "panic",
  "paranormal",
  "pastiche",
  "peculiar",
  "pell-mell",
  "performing",
  "perhaps",
  "perplexed",
  "perturb",
  "perverse",
  "perversion",
  "pervert",
  "phoney",
  "pickle",
  "pie",
  "pitching",
  "plastered",
  "plastic",
  "play",
  "playing",
  "pliably",
  "poorly",
  "pop",
  "possibly",
  "potential",
  "potty",
  "prancing",
  "preparation",
  "prepared",
  "problem",
  "processed",
  "produce",
  "pseudo",
  "pulverize",
  "pummelled",
  "put out",
  "put right",
  "put straight",
  "puzzling",
  "queer",
  "questionable",
  "quirky",
  "quivering",
  "ragged",
  "rambling",
  "rampage",
  "rampant",
  "randomly",
  "ravaged",
  "raving",
  "reaction",
  "rearranged",
  "reassembled",
  "rebuilt",
  "recast",
  "recollected",
  "recondition",
  "reconfigured",
  "reconstructed",
  "reconstruction",
  "recreated",
  "rectified",
  "recycled",
  "redeveloped",
  "redistributed",
  "redraft",
  "re-edit",
  "reeling",
  "refashioned",
  "refined",
  "refit",
  "reformed",
  "refurbished",
  "regenerated",
  "regulated",
  "rehash",
  "rejig",
  "remade",
  "remarkable",
  "remodel",
  "renovated",
  "rent",
  "reorder",
  "repackage",
  "repaired",
  "replaced",
  "repositioned",
  "resettled",
  "reshaped",
  "reshuffle",
  "resort",
  "restless",
  "restoration",
  "revamp",
  "reviewed",
  "revision",
  "revolting",
  "revolutionary",
  "rework",
  "rewritten",
  "ridiculous",
  "rig",
  "riotously",
  "rocky",
  "rogue",
  "rollicking",
  "rolls",
  "rotten",
  "roughly",
  "round",
  "rubbish",
  "ruin",
  "ruination",
  "rum",
  "run riot",
  "run wild",
  "ruptured",
  "sabotage",
  "sadly",
  "salad",
  "scattered",
  "scraggy",
  "scramble",
  "scrappy",
  "screw",
  "screwed",
  "screwy",
  "scruffy",
  "sculpture",
  "seedy",
  "served",
  "set",
  "set out",
  "shaken",
  "shambles",
  "shape",
  "shaped",
  "shattered",
  "shifty",
  "shot",
  "shown off",
  "shuffled",
  "sick",
  "silly",
  "sketchy",
  "slapdash",
  "slaughtered",
  "sloppy",
  "smashed",
  "snarled",
  "solution",
  "somehow",
  "sort",
  "sort of",
  "soup",
  "sozzled",
  "spasmodic",
  "spattered",
  "special",
  "spilled",
  "spinning",
  "splintered",
  "spoilt",
  "sprayed",
  "spread",
  "spun",
  "spurious",
  "squiffy",
  "staged",
  "staggering",
  "stew",
  "stormy",
  "strangely",
  "stray",
  "structured",
  "struggling",
  "stupidly",
  "styled",
  "stylistic",
  "substituted",
  "supply",
  "surprising",
  "suspect",
  "swilled",
  "swimming",
  "swirling",
  "switched",
  "synthetic",
  "tailor",
  "tainted",
  "tangle",
  "teetering",
  "terrible",
  "thrashing about",
  "tidy",
  "tight",
  "tipsy",
  "topsy-turvy",
  "tormented",
  "torn",
  "tortuous",
  "torture",
  "tortured",
  "trained",
  "transformed",
  "translated",
  "translation",
  "transmute",
  "transposed",
  "trashed",
  "treated",
  "treatment",
  "tricky",
  "trim",
  "trouble",
  "tumbling",
  "tumultuous",
  "turbulent",
  "turn",
  "turning",
  "tweaked",
  "twisted",
  "type",
  "ugly",
  "uncontrolled",
  "unconventional",
  "undone",
  "unexpected",
  "unfamiliar",
  "unkempt",
  "unnatural",
  "unorthodox",
  "unpredictable",
  "unravelling",
  "unrestrained",
  "unruly",
  "unscrambled",
  "unsettled",
  "unsound",
  "unstable",
  "unsteady",
  "untidy",
  "unusual",
  "upheaval",
  "upset",
  "vacillating",
  "vaguely",
  "vandalized",
  "variant",
  "variation",
  "variety",
  "various",
  "vary",
  "versatile",
  "version",
  "vigorously",
  "violated",
  "violently",
  "volatile",
  "wacky",
  "wandering",
  "warped",
  "wasted",
  "wayward",
  "weaved",
  "weird",
  "whip up",
  "whirl",
  "whisk",
  "wild",
  "winding",
  "work",
  "worked",
  "worried",
  "wound",
  "woven",
  "wrecked",
  "wrestle",
  "writhing",
  "wrong",
  "wrought",
  "yielding",
  "zany",
  "abandon",
  "aberration",
  "abominable",
  "abroad",
  "abstract",
  "abuse",
  "adapt",
  "adjust",
  "administer",
  "ado",
  "adulterate ",
  "adverse",
  "affect",
  "afflict",
  "aggravate",
  "aggrieve",
  "agitate",
  "ail",
  "alien",
  "alter",
  "amorphous ",
  "amuse",
  "analysis",
  "annoy",
  "anomaly",
  "antagonize",
  "antic",
  "anxious",
  "anyway",
  "approximate",
  "arbitrary",
  "architecture",
  "arouse",
  "art",
  "artifice",
  "assault",
  "assemble",
  "assort",
  "attack",
  "attempt",
  "atypical",
  "awry ",
  "babble",
  "baffle",
  "ballet",
  "basket-case",
  "battle",
  "beat",
  "befuddle",
  "bewilder",
  "bicker ",
  "bits",
  "blast",
  "blighted",
  "blithering",
  "blotto",
  "blow",
  "blunder",
  "bluster",
  "bob",
  "boil",
  "boisterous",
  "bomb",
  "booby",
  "boozy",
  "botch",
  "bother",
  "bounce",
  "bound",
  "bow",
  "box",
  "brawl",
  "breach",
  "break",
  "breeze",
  "brew",
  "broadcast ",
  "buckle",
  "budge",
  "buffet",
  "build",
  "bully",
  "bump",
  "bungle",
  "bustle",
  "calamity",
  "camouflage",
  "caper",
  "caprice",
  "career",
  "carry on",
  "carve",
  "casual",
  "cause",
  "cavalier",
  "cavort",
  "change",
  "characters",
  "chemistry",
  "chew",
  "chop",
  "chuck",
  "circulate",
  "circus",
  "clash",
  "clever",
  "cloud",
  "clumsy",
  "coarse",
  "collage",
  "collapse",
  "collate",
  "collect",
  "comedy",
  "comical",
  "commotion",
  "compile",
  "complicate",
  "compromise",
  "conceive",
  "concoct",
  "confection",
  "configure",
  "conflict",
  "confound",
  "confuse",
  "construe",
  "contentious",
  "contort",
  "contradict",
  "contrary",
  "contrast",
  "contrive",
  "controversy",
  "convert",
  "convolute",
  "convulse",
  "cook",
  "corked",
  "counterfeit",
  "cower",
  "crafty ",
  "cranky ",
  "crash",
  "create",
  "creep",
  "crime",
  "cripple",
  "crisis",
  "crumble",
  "crush",
  "cryptic",
  "cultivate",
  "culture",
  "cunning",
  "curdle",
  "cure",
  "cut and paste ",
  "damage",
  "dance",
  "dash",
  "deal",
  "debacle",
  "debauch",
  "debris",
  "decadent",
  "decay",
  "deceive",
  "decipher",
  "decline",
  "decode",
  "decompose",
  "deconstruct",
  "deform",
  "degenerate",
  "delinquent",
  "delude",
  "deplorable ",
  "deploy",
  "deprave",
  "derange",
  "derelict",
  "derive",
  "describe",
  "despair",
  "destroy",
  "deteriorate",
  "devastate",
  "develop",
  "deviate",
  "diabolical",
  "difficult",
  "digress",
  "dilapidated",
  "dilemma",
  "dire",
  "disaster",
  "discomfort",
  "discompose",
  "disconcert",
  "discord",
  "disease",
  "disengage",
  "disfigure",
  "disgrace",
  "disgruntled",
  "disguise",
  "disingenuous",
  "disjoint",
  "dislocate",
  "dislodge",
  "dismantle",
  "dismember",
  "disobey",
  "disorder",
  "disorganised",
  "disperse",
  "displace",
  "display",
  "dispose",
  "dispute",
  "disrepair",
  "disrupt",
  "dissect",
  "dissipate ",
  "dissolute",
  "dissolve",
  "distort",
  "distract",
  "distress",
  "distribute",
  "disturb",
  "dither",
  "divert",
  "dodge",
  "doubt",
  "drag",
  "dress",
  "drift",
  "drive",
  "eccentric",
  "ecstasy",
  "edgy",
  "edit",
  "else",
  "emerge",
  "energy",
  "engage",
  "enigma",
  "entertain",
  "entwine",
  "equivocal",
  "evolve",
  "exacerbate",
  "exceptional",
  "excite",
  "excursion",
  "execute",
  "exercise",
  "expatiate",
  "exploit",
  "extraordinary ",
  "fail",
  "fall",
  "falter",
  "fantasy",
  "fault",
  "fever",
  "fiasco",
  "fiddle",
  "fidget",
  "fierce",
  "figures",
  "finesse",
  "fitful",
  "fix",
  "fizzy",
  "flaky",
  "flap",
  "flaw",
  "flexible",
  "flicker",
  "flighty",
  "flimsy",
  "flirt",
  "flit",
  "floppy",
  "flounce",
  "flounder",
  "flourish",
  "fluster",
  "flux",
  "fly",
  "fold",
  "foreign",
  "forge",
  "formulate",
  "fracture",
  "fragile",
  "fragment",
  "free",
  "frightful",
  "frivolous",
  "frolic",
  "furore",
  "fury",
  "fuse",
  "fuss",
  "gad about",
  "gallivant",
  "gangling",
  "garble",
  "gauche",
  "gear",
  "generate",
  "germinate",
  "ghastly",
  "gibber",
  "giddy",
  "giggle",
  "glide",
  "gnarl",
  "gnash",
  "go",
  "googly",
  "grapple",
  "grate",
  "grave",
  "grief",
  "grind",
  "groggy",
  "gross",
  "group",
  "grow",
  "guise",
  "gyrate ",
  "hack",
  "haggard",
  "hallucination",
  "hammer",
  "handicraft",
  "handiwork",
  "handle",
  "harass",
  "harm",
  "harsh",
  "hatch",
  "have",
  "hazard",
  "haze",
  "heave",
  "helpless",
  "helter-skelter",
  "hesitant",
  "hook",
  "hop",
  "horrible",
  "horse around",
  "horseplay",
  "hotch-potch",
  "hover",
  "however",
  "hurl",
  "hurt",
  "hysteria ",
  "idiosyncratic",
  "ill",
  "ill-defined",
  "illegal",
  "illicit",
  "illusion",
  "imagine",
  "impair",
  "impede",
  "impersonate",
  "impolitic",
  "impromptu",
  "improvise",
  "in a way",
  "in solution",
  "incoherent",
  "incompetent",
  "incongruous",
  "incredible",
  "indecent",
  "indefinite",
  "indirect",
  "indispose",
  "indistinct",
  "individual",
  "indulge",
  "inebriate",
  "inexact",
  "infirm",
  "informal",
  "ingenius",
  "injure",
  "inquietude",
  "interfere",
  "interpret",
  "into",
  "intrigue",
  "invalid",
  "invent",
  "involve",
  "irresolute",
  "irritate",
  "itch ",
  "jam",
  "jar",
  "jargon",
  "jaunt",
  "jazz",
  "jerk",
  "jib",
  "jiggle",
  "jitter",
  "jive",
  "jockey",
  "joke",
  "jolly",
  "jolt",
  "jostle",
  "journey",
  "joust",
  "jump",
  "jungle",
  "just ",
  "kick",
  "kind",
  "kinematic",
  "kinetic",
  "kink",
  "knit",
  "knock",
  "knot ",
  "labour",
  "labyrinth",
  "lament",
  "languid",
  "lark",
  "latent",
  "latitude",
  "lay ",
  "laze",
  "leap",
  "legerdemain",
  "lenient",
  "liberal",
  "licence",
  "limber ",
  "lissom",
  "loll",
  "loose",
  "lounge ",
  "lousy",
  "lurch ",
  "Machiavellian",
  "machinate",
  "maelstrom",
  "magic",
  "malaise",
  "malcontent",
  "malformation",
  "malicious",
  "malleable",
  "maltreat",
  "manage",
  "manufacture",
  "marvel",
  "masquerade",
  "masticate",
  "material",
  "matter",
  "meddle",
  "melee",
  "melodrama",
  "melt",
  "menace",
  "mend",
  "mercurial",
  "merry",
  "mesh",
  "might be",
  "migrate",
  "mingle",
  "misbehave",
  "mischief",
  "misconduct",
  "misconstrue",
  "misdirect",
  "misery",
  "mishandle",
  "mishmash",
  "misrepresent",
  "misshape",
  "misty",
  "misunderstand",
  "mix ",
  "model",
  "moderate",
  "modify",
  "modulate",
  "monstrous",
  "motion",
  "motive",
  "mould",
  "muddle",
  "murky",
  "mush",
  "mutate",
  "mutilate",
  "mutiny",
  "mysterious ",
  "nasty",
  "neglect",
  "negotiate",
  "nervous ",
  "new ",
  "nimble",
  "nobble",
  "nomadic ",
  "nonconforming ",
  "nonplus",
  "nonsense",
  "nonstandard ",
  "nouveau ",
  "nutty ",
  "objectionable",
  "oblique",
  "obscene",
  "obscure",
  "offend",
  "onslaught",
  "operate",
  "option",
  "orchestra",
  "ordeal",
  "order",
  "organize",
  "orgy",
  "original",
  "oscillate",
  "ostensible ",
  "ought to be",
  "out of shape",
  "outwardly ",
  "overcome",
  "overhaul",
  "overt",
  "overthrow",
  "overturn",
  "overwhelm",
  "overwrought ",
  "palpitate",
  "pan",
  "paradox",
  "parody",
  "particular",
  "parts",
  "passion",
  "patch",
  "pathetic",
  "pattern",
  "peeve",
  "pelt",
  "penal",
  "perform",
  "perish",
  "perky",
  "permutation",
  "perpetrate",
  "perplex ",
  "persecute",
  "persuade",
  "petulant",
  "phony",
  "physical jerks",
  "pick out",
  "pick to pieces",
  "pickled",
  "piecemeal",
  "pieces",
  "pique",
  "pirouette",
  "pitch",
  "pitiful",
  "pivot",
  "place",
  "plight ",
  "plot",
  "ply",
  "poke",
  "pooh pooh",
  "poor",
  "pose",
  "position",
  "possible",
  "pound",
  "prance",
  "prank",
  "precarious",
  "predicament",
  "prepare",
  "press",
  "pretend",
  "primitive",
  "problem ",
  "process ",
  "profligate",
  "program",
  "propel",
  "protest",
  "provoke",
  "pull into line",
  "pull to pieces",
  "pulp",
  "pummel",
  "punch",
  "punish",
  "push",
  "put",
  "put in order",
  "put off",
  "puzzle ",
  "quaint",
  "quake",
  "quandary",
  "quarrel",
  "question",
  "quiver ",
  "rabble",
  "rack",
  "radical",
  "raffish",
  "rail",
  "raillery",
  "rally",
  "ramble",
  "ramshackle",
  "random",
  "range",
  "rare",
  "rattle",
  "ratty",
  "ravage",
  "raw",
  "rearrange",
  "reassemble",
  "rebel",
  "rebuild",
  "recipe ",
  "reckless",
  "recollect",
  "recombine",
  "reconsider",
  "reconstruct",
  "recreate",
  "rectify",
  "recycle",
  "redesign",
  "redevelop",
  "redirect",
  "rediscover",
  "redraw",
  "reel",
  "reform",
  "regenerate",
  "regrettable",
  "reincarnate",
  "relaunch",
  "relax",
  "relay",
  "relocate",
  "rend ",
  "render",
  "renew",
  "renovate",
  "reorganise",
  "repair",
  "replace",
  "represent",
  "reproduce",
  "resettle",
  "resolve",
  "restitution",
  "restore",
  "restructure",
  "review",
  "revise",
  "revolt",
  "revolve",
  "rewrite",
  "rickety",
  "riddle",
  "ridicule ",
  "riot",
  "rip",
  "ripple",
  "roam",
  "rock",
  "roll",
  "rough",
  "rouse",
  "rove",
  "row",
  "ruffle",
  "rumble",
  "run",
  "rupture",
  "rustic ",
  "sack",
  "sad",
  "savage",
  "scandal",
  "scathe",
  "scatter",
  "scheme",
  "scrap",
  "scuffle",
  "seesaw",
  "seizure",
  "serve",
  "set in motion",
  "set right",
  "shake",
  "sham",
  "shatter",
  "shift ",
  "shimmer",
  "shimmy ",
  "shiver",
  "shock",
  "shoddy",
  "shove",
  "shred",
  "shredded",
  "shudder",
  "shuffle",
  "singular",
  "sinuous",
  "skip",
  "skittish",
  "sleight",
  "slide",
  "slip",
  "smack",
  "smash",
  "smudge",
  "solve",
  "somersault",
  "sophisticated",
  "sorry",
  "spank",
  "speculate",
  "spill",
  "spin",
  "spirited",
  "split",
  "spoil",
  "sport",
  "sprawl",
  "spray",
  "sprightly",
  "spring",
  "sprinkle",
  "squirm",
  "stagger",
  "stampede",
  "startle ",
  "stir",
  "storm",
  "straighten",
  "strange",
  "strategy",
  "struggle",
  "stumble",
  "stunt",
  "stupid",
  "style",
  "substance",
  "suffer",
  "supple",
  "surgery",
  "surprise",
  "swagger",
  "sway",
  "swim",
  "swing",
  "swirl",
  "switch ",
  "tackle",
  "tactics",
  "take badly",
  "take by surprise",
  "take ill",
  "take liberties",
  "take wing",
  "tamper",
  "tawdry",
  "tear",
  "tease",
  "teeter",
  "temperamental",
  "tempered",
  "theatrical",
  "thrash",
  "threaten",
  "throb",
  "throw",
  "tickle",
  "tinker",
  "tongue in cheek",
  "torment",
  "toss",
  "totter",
  "touchy",
  "tour",
  "tousle",
  "toy",
  "tragic",
  "train",
  "transfer",
  "transform",
  "transition",
  "translate",
  "transport",
  "transpose",
  "trash ",
  "travel",
  "treachery",
  "treat",
  "tremble",
  "trepidation",
  "trial",
  "tribulation",
  "trick",
  "trip",
  "tumble",
  "tumult",
  "turmoil",
  "tweak",
  "twirl",
  "twist",
  "twitch ",
  "unbalanced",
  "unbridled",
  "unbundle",
  "uncanny",
  "uncertain",
  "uncivilised",
  "uncultivated",
  "uncured",
  "undecided",
  "undefined",
  "undisciplined",
  "undulate",
  "unearthly",
  "unfaithful",
  "unfettered",
  "unfit",
  "unfortunate",
  "unfurl",
  "ungainly",
  "ungoverned",
  "unhappy",
  "unhealthy",
  "unlawful",
  "unlikely",
  "unordered",
  "unprepared",
  "unravel",
  "unrecognisable",
  "unrest",
  "unrestricted",
  "unscrupulous",
  "unseemly",
  "unsettle",
  "untamed",
  "untethered",
  "untoward",
  "untrained",
  "untrue",
  "untutored",
  "unwieldy",
  "up in the air",
  "uprise",
  "uproar",
  "upset ",
  "vacillate",
  "vagrant",
  "vague",
  "veer",
  "venture",
  "vex",
  "vibrate",
  "vigorous",
  "violent",
  "wag",
  "wander",
  "warp",
  "wave",
  "waver",
  "weird ",
  "whack",
  "wicked",
  "wiggle",
  "wind",
  "wise",
  "wobble",
  "wonder",
  "woolly",
  "worry",
  "wreck",
  "wretched",
  "wriggle",
  "wring",
  "writhe",
  "preparation of",
  "in resort"]
