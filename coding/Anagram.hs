module Anagram where

import qualified Data.Set 

import Utils

isAnagramWord :: [String] -> Bool
isAnagramWord xs = Data.Set.member (unwords xs) anagramIndicators


anagramIndicators = Data.Set.fromList [
  "amended",
  "advantage",
  "abandoned",
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