module Indicators
  ( isDefIndicator
  , isJuxtIndicator
  , isReverseJuxtIndicator
  , isAnagIndicator
  , isInsertIndicator
  , isReverseInsertIndicator
  , isSubIndicator
  , isReverseSubIndicator
  , isHiddenIndicator
  , isReverseIndicator
  , isFirstsIndicator
  , isLastsIndicator
  , isPartIndicator
  , isBeforeIndicator
  , isAfterIndicator
  ) where

import qualified Data.Set as S

import Types

isDefIndicator,
  isJuxtIndicator,
  isReverseJuxtIndicator,
  isAnagIndicator,
  isInsertIndicator,
  isReverseInsertIndicator,
  isSubIndicator,
  isReverseSubIndicator,
  isHiddenIndicator,
  isReverseIndicator,
  isFirstsIndicator,
  isLastsIndicator,
  isPartIndicator,
  isBeforeIndicator,
  isAfterIndicator :: Words -> Bool

isDefIndicator            = isIndicatorIn defIndicators
isJuxtIndicator           = isIndicatorIn juxtIndicators
isReverseJuxtIndicator    = isIndicatorIn reverseJuxtIndicators
isAnagIndicator           = isIndicatorIn anagIndicators
isInsertIndicator         = isIndicatorIn insertIndicators
isReverseInsertIndicator  = isIndicatorIn reverseInsertIndicators
isSubIndicator            = isIndicatorIn subIndicators
isReverseSubIndicator     = isIndicatorIn reverseSubIndicators
isHiddenIndicator         = isIndicatorIn hiddenIndicators
isReverseIndicator        = isIndicatorIn reverseIndicators
isFirstsIndicator         = isIndicatorIn firstsIndicators
isLastsIndicator          = isIndicatorIn lastsIndicators
isPartIndicator           = isIndicatorIn partsIndicators
isBeforeIndicator         = isIndicatorIn beforeIndicators
isAfterIndicator          = isIndicatorIn afterIndicators

isIndicatorIn :: S.Set Phrase -> Words -> Bool
isIndicatorIn s ws
  = S.member (unwords ws) s

defIndicators,
  juxtIndicators,
  reverseJuxtIndicators,
  anagIndicators,
  insertIndicators,
  reverseInsertIndicators,
  subIndicators,
  reverseSubIndicators,
  hiddenIndicators,
  reverseIndicators,
  firstsIndicators,
  lastsIndicators,
  partsIndicators,
  beforeIndicators,
  afterIndicators :: S.Set Phrase

defIndicators
  = S.fromList
      [ "and"
      , "brings out"
      , "for a"
      , "for"
      , "give"
      , "in"
      , "is"
      , "made by"
      , "makes"
      , "providing"
      , "show"
      , "to get"
      , "to give"
      , "to make a"
      , "to"
      , "with"
      ]

juxtIndicators
  = S.fromList
      [ "above"
      , "beside"
      , "next to"
      , "over"
      , "to"
      , "on"
      , "with"
      ]

reverseJuxtIndicators
  = S.fromList
      [ "after"
      , "opposite"
      , "with"
      ]

anagIndicators
  = S.fromList
      [ "moister"
      , "Byzantine"
      , "Machiavellian"
      , "abandon"
      , "abandoned"
      , "aberration"
      , "aberrations"
      , "abnormal"
      , "abominable"
      , "about"
      , "abroad"
      , "abstract"
      , "absurd"
      , "abuse"
      , "abused"
      , "abysmal"
      , "accident"
      , "acrobatics"
      , "action"
      , "active"
      , "adapt"
      , "adapted"
      , "addled"
      , "adjust"
      , "adjusted"
      , "administer"
      , "ado"
      , "adrift"
      , "adulterate"
      , "advantage"
      , "adverse"
      , "affect"
      , "affected"
      , "afflict"
      , "afresh"
      , "after a fashion"
      , "aggravate"
      , "aggrieve"
      , "agile"
      , "agitate"
      , "agitated"
      , "ail"
      , "alien"
      , "all over the place"
      , "alter"
      , "altered"
      , "alternative"
      , "amazing"
      , "amend"
      , "amendment"
      , "amiss"
      , "amok"
      , "amorphous"
      , "amuse"
      , "analysis"
      , "anarchic"
      , "anew"
      , "animated"
      , "annoy"
      , "anomalous"
      , "anomaly"
      , "another"
      , "antagonize"
      , "antic"
      , "anxious"
      , "anyhow"
      , "anyway"
      , "approximate"
      , "arbitrary"
      , "architecture"
      , "around"
      , "arouse"
      , "arrange"
      , "arrangement"
      , "art"
      , "artful"
      , "artifice"
      , "askew"
      , "assault"
      , "assemble"
      , "assembled"
      , "assort"
      , "assorted"
      , "astray"
      , "at odds"
      , "at random"
      , "at sea"
      , "at sixes and sevens"
      , "atrocious"
      , "attack"
      , "attempt"
      , "atypical"
      , "awful"
      , "awkward"
      , "awry"
      , "babble"
      , "bad"
      , "baffle"
      , "baffled"
      , "bake"
      , "ballet"
      , "bananas"
      , "barking"
      , "basket-case"
      , "bastardization"
      , "bats"
      , "battered"
      , "battle"
      , "batty"
      , "beat"
      , "beaten up"
      , "beaten"
      , "become"
      , "bedlam"
      , "befuddle"
      , "belted"
      , "bend"
      , "bendy"
      , "bent"
      , "berserk"
      , "bespoke"
      , "bewilder"
      , "bicker"
      , "bits"
      , "bizarre"
      , "blast"
      , "blasted"
      , "blazing"
      , "blend"
      , "blighted"
      , "blithering"
      , "blotto"
      , "blow"
      , "blown up"
      , "blunder"
      , "bluster"
      , "bob"
      , "boil"
      , "boiled"
      , "boisterous"
      , "bomb"
      , "booby"
      , "boozy"
      , "botch"
      , "bother"
      , "bounce"
      , "bound"
      , "bow"
      , "box"
      , "brawl"
      , "breach"
      , "break up"
      , "break"
      , "breeze"
      , "brew"
      , "brewed"
      , "broach"
      , "broadcast"
      , "broken"
      , "broth"
      , "buckle"
      , "buckled"
      , "budge"
      , "buffalo"
      , "buffet"
      , "build"
      , "building"
      , "built"
      , "bully"
      , "bump"
      , "bungle"
      , "bungled"
      , "burst"
      , "bust"
      , "bustle"
      , "bustling"
      , "busy"
      , "butcher"
      , "by arrangement"
      , "by mistake"
      , "calamity"
      , "camouflage"
      , "can be"
      , "caper"
      , "caprice"
      , "career"
      , "careless"
      , "carry on"
      , "carve"
      , "carved up"
      , "cast"
      , "casual"
      , "cause"
      , "cavalier"
      , "cavort"
      , "cavorting"
      , "change"
      , "changed"
      , "changes shape"
      , "changing"
      , "chaotic"
      , "characters"
      , "chemistry"
      , "chew"
      , "chop"
      , "chopped up"
      , "choppy"
      , "chuck"
      , "churn"
      , "circulate"
      , "circulated"
      , "circus"
      , "clash"
      , "clever"
      , "clobber"
      , "cloud"
      , "clumsy"
      , "coarse"
      , "cock-up"
      , "cocktail"
      , "code"
      , "collage"
      , "collapse"
      , "collate"
      , "collect"
      , "comedy"
      , "comical"
      , "commotion"
      , "compilation"
      , "compile"
      , "complex"
      , "complicate"
      , "complicated"
      , "compose"
      , "composition"
      , "compound"
      , "comprising"
      , "compromise"
      , "concealed"
      , "conceive"
      , "concoct"
      , "concoction"
      , "confection"
      , "configure"
      , "conflict"
      , "confound"
      , "confuse"
      , "confused"
      , "constituents"
      , "construction"
      , "construe"
      , "contentious"
      , "contort"
      , "contorted"
      , "contradict"
      , "contrary"
      , "contrast"
      , "contrive"
      , "contrived"
      , "controversy"
      , "conversion"
      , "convert"
      , "converted"
      , "convolute"
      , "convoluted"
      , "convulse"
      , "convulsing"
      , "cook"
      , "cooked"
      , "corked"
      , "correct"
      , "correction"
      , "corrupt"
      , "could be"
      , "counterfeit"
      , "cower"
      , "crack"
      , "cracked up"
      , "cracked"
      , "crackers"
      , "craftily"
      , "crafty"
      , "cranky"
      , "crash"
      , "crashed"
      , "crazy"
      , "create"
      , "creep"
      , "crime"
      , "criminal"
      , "cripple"
      , "crisis"
      , "crooked"
      , "crude"
      , "crumble"
      , "crumbly"
      , "crush"
      , "crushed"
      , "cryptic"
      , "cuckoo"
      , "cultivate"
      , "culture"
      , "cunning"
      , "cunningly"
      , "curdle"
      , "cure"
      , "curious"
      , "cut and paste"
      , "daft"
      , "damage"
      , "damaged"
      , "dance"
      , "dancing"
      , "dash"
      , "dashing"
      , "deal"
      , "debacle"
      , "debauch"
      , "debris"
      , "decadent"
      , "decay"
      , "deceive"
      , "decipher"
      , "decline"
      , "decode"
      , "decompose"
      , "deconstruct"
      , "defective"
      , "deform"
      , "deformed"
      , "degenerate"
      , "delinquent"
      , "delirious"
      , "delude"
      , "demented"
      , "demolish"
      , "deplorable"
      , "deploy"
      , "deployed"
      , "deprave"
      , "derange"
      , "deranged"
      , "derelict"
      , "derive"
      , "derived from"
      , "describe"
      , "design"
      , "despair"
      , "desperate"
      , "destroy"
      , "destroyed"
      , "destruction"
      , "desultory"
      , "deteriorate"
      , "devastate"
      , "devastated"
      , "develop"
      , "developed"
      , "deviant"
      , "deviate"
      , "devilish"
      , "devious"
      , "diabolical"
      , "dicky"
      , "different"
      , "difficult"
      , "digress"
      , "dilapidated"
      , "dilemma"
      , "dire"
      , "disarray"
      , "disaster"
      , "discomfort"
      , "discompose"
      , "disconcert"
      , "discord"
      , "disease"
      , "disengage"
      , "disfigure"
      , "disfigured"
      , "disgrace"
      , "disgruntled"
      , "disguise"
      , "disguised"
      , "dished out"
      , "dishevelled"
      , "disingenuous"
      , "disintegrating"
      , "disjoint"
      , "disjointed"
      , "dislocate"
      , "dislocated"
      , "dislodge"
      , "dismantle"
      , "dismantled"
      , "dismember"
      , "disobey"
      , "disorder"
      , "disorderly"
      , "disorganised"
      , "disorganized"
      , "disorientated"
      , "disperse"
      , "dispersed"
      , "displace"
      , "display"
      , "dispose"
      , "disposed"
      , "dispute"
      , "disrepair"
      , "disrupt"
      , "disrupted"
      , "dissect"
      , "disseminated"
      , "dissipate"
      , "dissolute"
      , "dissolve"
      , "dissolving"
      , "distilled"
      , "distort"
      , "distorted"
      , "distract"
      , "distraught"
      , "distress"
      , "distressed"
      , "distribute"
      , "disturb"
      , "disturbance"
      , "disturbed"
      , "dither"
      , "diverse"
      , "divert"
      , "dizzy"
      , "doctor"
      , "doddery"
      , "dodge"
      , "dodgy"
      , "done"
      , "dotty"
      , "doubt"
      , "drag"
      , "dreadful"
      , "dress"
      , "dressed"
      , "drift"
      , "drifting"
      , "drive"
      , "drunk"
      , "dubious"
      , "dud"
      , "duff"
      , "dynamic"
      , "easily"
      , "eccentric"
      , "eccentried"
      , "ecstasy"
      , "edgy"
      , "edit"
      , "elaborate"
      , "elastic"
      , "else"
      , "embroil"
      , "emendation"
      , "emerge"
      , "energy"
      , "engage"
      , "engendering"
      , "engineer"
      , "enigma"
      , "ensemble"
      , "entangle"
      , "entertain"
      , "entwine"
      , "equivocal"
      , "err"
      , "errant"
      , "erratic"
      , "erroneous"
      , "erupt"
      , "eruption"
      , "evolution"
      , "evolve"
      , "exacerbate"
      , "exceptional"
      , "exchange"
      , "exchanged"
      , "excite"
      , "excited"
      , "excursion"
      , "execute"
      , "exercise"
      , "exercising"
      , "exotic"
      , "expatiate"
      , "explode"
      , "exploit"
      , "explosive"
      , "extraordinary"
      , "extravagant"
      , "fabricated"
      , "fail"
      , "fake"
      , "fall"
      , "false"
      , "falter"
      , "fanatical"
      , "fancy"
      , "fantastic"
      , "fantasy"
      , "fashion"
      , "fault"
      , "faulty"
      , "fermented"
      , "fever"
      , "fiasco"
      , "fickle"
      , "fiddle"
      , "fiddled"
      , "fidget"
      , "fierce"
      , "figures"
      , "finesse"
      , "fishy"
      , "fitful"
      , "fix"
      , "fixed"
      , "fizzy"
      , "flailing"
      , "flaky"
      , "flap"
      , "flaw"
      , "flexible"
      , "flexibly"
      , "flicker"
      , "flighty"
      , "flimsy"
      , "flirt"
      , "flit"
      , "floppy"
      , "flounce"
      , "flounder"
      , "floundering"
      , "flourish"
      , "flow"
      , "fluctuate"
      , "fluid"
      , "flurry"
      , "fluster"
      , "flustered"
      , "flux"
      , "fly"
      , "flying"
      , "fold"
      , "foolish"
      , "for a change"
      , "for revision"
      , "forced"
      , "foreign"
      , "forge"
      , "forged"
      , "form"
      , "formulate"
      , "foul"
      , "fracture"
      , "fractured"
      , "fragile"
      , "fragment"
      , "fragmented"
      , "frantic"
      , "freak"
      , "freaking out"
      , "free"
      , "freely"
      , "frenetic"
      , "frenzied"
      , "frenzy"
      , "fresh"
      , "freshly"
      , "frightful"
      , "frightfully"
      , "frisky"
      , "frivolous"
      , "frolic"
      , "frolicsome"
      , "fuddled"
      , "fudge"
      , "funny"
      , "furiously"
      , "furore"
      , "fury"
      , "fuse"
      , "fuss"
      , "gad about"
      , "gaffe"
      , "gallivant"
      , "gambol"
      , "gangling"
      , "garble"
      , "garbled"
      , "gauche"
      , "gear"
      , "generate"
      , "generating"
      , "germinate"
      , "gets"
      , "ghastly"
      , "gibber"
      , "giddily"
      , "giddy"
      , "giggle"
      , "gingerly"
      , "giving"
      , "glide"
      , "gnarl"
      , "gnash"
      , "go off"
      , "go"
      , "going crazy"
      , "gone"
      , "googly"
      , "grapple"
      , "grate"
      , "grave"
      , "grief"
      , "grim"
      , "grind"
      , "groggy"
      , "groomed"
      , "gross"
      , "grotesque"
      , "ground"
      , "group"
      , "grow"
      , "guise"
      , "gyrate"
      , "hack"
      , "haggard"
      , "hallucination"
      , "hammer"
      , "hammered"
      , "handicraft"
      , "handiwork"
      , "handle"
      , "haphazard"
      , "harass"
      , "harassed"
      , "harm"
      , "harmed"
      , "harsh"
      , "hash"
      , "hatch"
      , "have"
      , "havoc"
      , "haywire"
      , "hazard"
      , "haze"
      , "heave"
      , "hectic"
      , "helpless"
      , "helter-skelter"
      , "hesitant"
      , "hideous"
      , "higgledy-piggledy"
      , "high"
      , "hit"
      , "hook"
      , "hop"
      , "horrible"
      , "horribly"
      , "horrid"
      , "horrific"
      , "horse around"
      , "horseplay"
      , "hotch-potch"
      , "hotchpotch"
      , "hover"
      , "however"
      , "hurl"
      , "hurt"
      , "hybrid"
      , "hysteria"
      , "idiosyncratic"
      , "idiotic"
      , "ill"
      , "ill-bred"
      , "ill-defined"
      , "ill-disposed"
      , "ill-formed"
      , "ill-treated"
      , "illegal"
      , "illicit"
      , "illusion"
      , "imagine"
      , "impair"
      , "impede"
      , "imperfect"
      , "impersonate"
      , "impolitic"
      , "impromptu"
      , "improper"
      , "improvise"
      , "impure"
      , "in a ferment"
      , "in a jumble"
      , "in a mess"
      , "in a way"
      , "in a whirl"
      , "in disarray"
      , "in disguise"
      , "in error"
      , "in motion"
      , "in pieces"
      , "in resort"
      , "in ruins"
      , "in solution"
      , "inaccurate"
      , "incoherent"
      , "incompetent"
      , "incongruous"
      , "incorrect"
      , "incredible"
      , "indecent"
      , "indefinite"
      , "indirect"
      , "indispose"
      , "indistinct"
      , "individual"
      , "indulge"
      , "inebriate"
      , "inebriated"
      , "inept"
      , "inexact"
      , "infirm"
      , "informal"
      , "ingenius"
      , "ingredients"
      , "injure"
      , "injured"
      , "inordinate"
      , "inquietude"
      , "insanely"
      , "interfere"
      , "interfered with"
      , "interpret"
      , "into"
      , "intoxicated"
      , "intricate"
      , "intrigue"
      , "invalid"
      , "invent"
      , "invention"
      , "involve"
      , "involved"
      , "irregular"
      , "irresolute"
      , "irritate"
      , "irritated"
      , "itch"
      , "itinerant"
      , "jam"
      , "jar"
      , "jargon"
      , "jaunt"
      , "jaunty"
      , "jazz"
      , "jazzy"
      , "jerk"
      , "jerky"
      , "jib"
      , "jiggle"
      , "jiggling"
      , "jitter"
      , "jittery"
      , "jive"
      , "jockey"
      , "joke"
      , "jolly"
      , "jolt"
      , "jolting"
      , "jostle"
      , "jostling"
      , "journey"
      , "joust"
      , "juddering"
      , "juggle"
      , "jumble"
      , "jumbled"
      , "jump"
      , "jumping"
      , "jungle"
      , "junk"
      , "just"
      , "kick"
      , "kind of"
      , "kind"
      , "kinematic"
      , "kinetic"
      , "kink"
      , "kinky"
      , "knead"
      , "knit"
      , "knock"
      , "knocked about"
      , "knot"
      , "knotted"
      , "labour"
      , "laboured"
      , "labyrinth"
      , "labyrinthine"
      , "lament"
      , "languid"
      , "lark"
      , "latent"
      , "latitude"
      , "lawless"
      , "lax"
      , "lay"
      , "laze"
      , "leap"
      , "legerdemain"
      , "lenient"
      , "liberal"
      , "liberally"
      , "licence"
      , "limber"
      , "liquid"
      , "lissom"
      , "lively"
      , "loll"
      , "loony"
      , "loose"
      , "loosely"
      , "lost"
      , "lounge"
      , "lousy"
      , "ludicrous"
      , "lunatic"
      , "lurch"
      , "machinate"
      , "mad"
      , "made-up"
      , "madness"
      , "maelstrom"
      , "magic"
      , "make changes in"
      , "make changes"
      , "make up"
      , "make"
      , "malaise"
      , "malcontent"
      , "malformation"
      , "malformed"
      , "malfunction"
      , "malicious"
      , "malleable"
      , "malleably"
      , "maltreat"
      , "maltreated"
      , "manage"
      , "managed"
      , "mangle"
      , "manic"
      , "manipulate"
      , "manoeuvre"
      , "manufacture"
      , "manufacturing"
      , "mar"
      , "marvel"
      , "mash"
      , "mashed"
      , "masquerade"
      , "massage"
      , "masticate"
      , "material"
      , "matter"
      , "maul"
      , "maybe"
      , "mayhem"
      , "meandering"
      , "meddle"
      , "medley"
      , "melange"
      , "melee"
      , "melodrama"
      , "melt"
      , "melted"
      , "menace"
      , "mend"
      , "mental"
      , "mercurial"
      , "merry"
      , "mesh"
      , "mess"
      , "metamorphosis"
      , "might be"
      , "migrate"
      , "mince"
      , "mingle"
      , "mint"
      , "misbehave"
      , "misbehaves"
      , "misbehaving"
      , "mischief"
      , "mischievously"
      , "misconduct"
      , "misconstrue"
      , "misdirect"
      , "misery"
      , "misfit"
      , "misguided"
      , "mishandle"
      , "mishandled"
      , "mishap"
      , "mishmash"
      , "misinterpret"
      , "mislead"
      , "mismanagement"
      , "misplace"
      , "misprint"
      , "misrepresent"
      , "misrepresented"
      , "misshape"
      , "misshapen"
      , "misspell"
      , "mistake"
      , "mistreat"
      , "misty"
      , "misunderstand"
      , "misuse"
      , "mix"
      , "mix-up"
      , "mixed up"
      , "mixture"
      , "mobile"
      , "mobilize"
      , "model"
      , "modelled"
      , "moderate"
      , "modification"
      , "modify"
      , "modulate"
      , "mongrel"
      , "monstrous"
      , "motion"
      , "motive"
      , "mould"
      , "mount"
      , "move"
      , "movement"
      , "moving"
      , "muddle"
      , "muddled"
      , "muff"
      , "munching"
      , "murky"
      , "mush"
      , "mushy"
      , "mutate"
      , "mutation"
      , "mutilate"
      , "mutilation"
      , "mutinous"
      , "mutiny"
      , "mysterious"
      , "mêlée"
      , "nastily"
      , "nasty"
      , "naughty"
      , "neatened"
      , "neglect"
      , "negotiate"
      , "nervous"
      , "new"
      , "nimble"
      , "nobble"
      , "nomadic"
      , "nonconforming"
      , "nonplus"
      , "nonsense"
      , "nonstandard"
      , "not in order"
      , "not properly"
      , "not right"
      , "nouveau"
      , "novel"
      , "nuts"
      , "nutty"
      , "objectionable"
      , "oblique"
      , "obscene"
      , "obscure"
      , "obstreperous"
      , "odd"
      , "oddball"
      , "off"
      , "offend"
      , "onslaught"
      , "operate"
      , "option"
      , "orchestra"
      , "ordeal"
      , "order"
      , "orderly"
      , "organization"
      , "organize"
      , "organized"
      , "orgy"
      , "original"
      , "originally"
      , "ornate"
      , "oscillate"
      , "ostensible"
      , "otherwise"
      , "ought to be"
      , "out of hand"
      , "out of joint"
      , "out of order"
      , "out of place"
      , "out of shape"
      , "out of sorts"
      , "out"
      , "outlandish"
      , "outrageous"
      , "outré"
      , "outwardly"
      , "overcome"
      , "overhaul"
      , "overt"
      , "overthrow"
      , "overturn"
      , "overwhelm"
      , "overwrought"
      , "palpitate"
      , "pan"
      , "panic"
      , "paradox"
      , "paranormal"
      , "parody"
      , "particular"
      , "parts"
      , "passion"
      , "pastiche"
      , "patch"
      , "pathetic"
      , "pattern"
      , "peculiar"
      , "peeve"
      , "pell-mell"
      , "pelt"
      , "penal"
      , "perform"
      , "performing"
      , "perhaps"
      , "perish"
      , "perky"
      , "permutation"
      , "perpetrate"
      , "perplex"
      , "perplexed"
      , "persecute"
      , "persuade"
      , "perturb"
      , "perverse"
      , "perversion"
      , "pervert"
      , "petulant"
      , "phoney"
      , "phony"
      , "physical jerks"
      , "pick out"
      , "pick to pieces"
      , "pickle"
      , "pickled"
      , "pie"
      , "piecemeal"
      , "pieces"
      , "pique"
      , "pirouette"
      , "pitch"
      , "pitching"
      , "pitiful"
      , "pivot"
      , "place"
      , "plastered"
      , "plastic"
      , "play"
      , "playing"
      , "pliably"
      , "plight"
      , "plot"
      , "ply"
      , "poke"
      , "pooh pooh"
      , "poor"
      , "poorly"
      , "pop"
      , "pose"
      , "position"
      , "possible"
      , "possibly"
      , "potential"
      , "potty"
      , "pound"
      , "prance"
      , "prancing"
      , "prank"
      , "precarious"
      , "predicament"
      , "preparation of"
      , "preparation"
      , "prepare"
      , "prepared"
      , "press"
      , "pretend"
      , "primitive"
      , "problem"
      , "process"
      , "processed"
      , "produce"
      , "profligate"
      , "program"
      , "propel"
      , "protest"
      , "provoke"
      , "pseudo"
      , "pull into line"
      , "pull to pieces"
      , "pulp"
      , "pulverize"
      , "pummel"
      , "pummelled"
      , "punch"
      , "punish"
      , "push"
      , "put in order"
      , "put off"
      , "put out"
      , "put right"
      , "put straight"
      , "put"
      , "puzzle"
      , "puzzling"
      , "quaint"
      , "quake"
      , "quandary"
      , "quarrel"
      , "queer"
      , "question"
      , "questionable"
      , "quirky"
      , "quiver"
      , "quivering"
      , "rabble"
      , "rack"
      , "radical"
      , "raffish"
      , "ragged"
      , "rail"
      , "raillery"
      , "rally"
      , "ramble"
      , "rambling"
      , "rampage"
      , "rampant"
      , "ramshackle"
      , "random"
      , "randomly"
      , "range"
      , "rare"
      , "rattle"
      , "ratty"
      , "ravage"
      , "ravaged"
      , "raving"
      , "raw"
      , "re-edit"
      , "reaction"
      , "rearrange"
      , "rearranged"
      , "reassemble"
      , "reassembled"
      , "rebel"
      , "rebuild"
      , "rebuilt"
      , "recast"
      , "recipe"
      , "reckless"
      , "recollect"
      , "recollected"
      , "recombine"
      , "recondition"
      , "reconfigured"
      , "reconsider"
      , "reconstruct"
      , "reconstructed"
      , "reconstruction"
      , "recreate"
      , "recreated"
      , "rectified"
      , "rectify"
      , "recycle"
      , "recycled"
      , "redesign"
      , "redevelop"
      , "redeveloped"
      , "redirect"
      , "rediscover"
      , "redistributed"
      , "redraft"
      , "redraw"
      , "reel"
      , "reeling"
      , "refashioned"
      , "refined"
      , "refit"
      , "reform"
      , "reformed"
      , "refurbished"
      , "regenerate"
      , "regenerated"
      , "regrettable"
      , "regulated"
      , "rehash"
      , "reincarnate"
      , "rejig"
      , "relaunch"
      , "relax"
      , "relay"
      , "relocate"
      , "remade"
      , "remarkable"
      , "remodel"
      , "rend"
      , "render"
      , "renew"
      , "renovate"
      , "renovated"
      , "rent"
      , "reorder"
      , "reorganise"
      , "repackage"
      , "repair"
      , "repaired"
      , "replace"
      , "replaced"
      , "repositioned"
      , "represent"
      , "reproduce"
      , "resettle"
      , "resettled"
      , "reshaped"
      , "reshuffle"
      , "resolve"
      , "resort"
      , "restitution"
      , "restless"
      , "restoration"
      , "restore"
      , "restructure"
      , "revamp"
      , "review"
      , "reviewed"
      , "revise"
      , "revision"
      , "revolt"
      , "revolting"
      , "revolutionary"
      , "revolve"
      , "rework"
      , "rewrite"
      , "rewritten"
      , "rickety"
      , "riddle"
      , "ridicule"
      , "ridiculous"
      , "rig"
      , "riot"
      , "riotously"
      , "rip"
      , "ripple"
      , "roam"
      , "rock"
      , "rocky"
      , "rogue"
      , "roll"
      , "rollicking"
      , "rolls"
      , "rotten"
      , "rough"
      , "roughly"
      , "round"
      , "rouse"
      , "rove"
      , "row"
      , "rubbish"
      , "ruffle"
      , "ruin"
      , "ruination"
      , "rum"
      , "rumble"
      , "run riot"
      , "run wild"
      , "run"
      , "rupture"
      , "ruptured"
      , "rustic"
      , "sabotage"
      , "sack"
      , "sad"
      , "sadly"
      , "salad"
      , "savage"
      , "scandal"
      , "scathe"
      , "scatter"
      , "scattered"
      , "scheme"
      , "scraggy"
      , "scramble"
      , "scrap"
      , "scrappy"
      , "screw"
      , "screwed"
      , "screwy"
      , "scruffy"
      , "scuffle"
      , "sculpture"
      , "seedy"
      , "seesaw"
      , "seizure"
      , "serve"
      , "served"
      , "set in motion"
      , "set out"
      , "set right"
      , "set"
      , "shake"
      , "shaken"
      , "sham"
      , "shambles"
      , "shape"
      , "shaped"
      , "shatter"
      , "shattered"
      , "shift"
      , "shifty"
      , "shimmer"
      , "shimmy"
      , "shiver"
      , "shock"
      , "shoddy"
      , "shot"
      , "shove"
      , "shown off"
      , "shred"
      , "shredded"
      , "shudder"
      , "shuffle"
      , "shuffled"
      , "sick"
      , "silly"
      , "singular"
      , "sinuous"
      , "sketchy"
      , "skip"
      , "skittish"
      , "slapdash"
      , "slaughtered"
      , "sleight"
      , "slide"
      , "slip"
      , "sloppy"
      , "smack"
      , "smash"
      , "smashed"
      , "smudge"
      , "snarled"
      , "solution"
      , "solve"
      , "somehow"
      , "somersault"
      , "sophisticated"
      , "sorry"
      , "sort of"
      , "sort"
      , "soup"
      , "sozzled"
      , "spank"
      , "spasmodic"
      , "spattered"
      , "special"
      , "speculate"
      , "spill"
      , "spilled"
      , "spin"
      , "spinning"
      , "spirited"
      , "splintered"
      , "split"
      , "spoil"
      , "spoilt"
      , "sport"
      , "sprawl"
      , "spray"
      , "sprayed"
      , "spread"
      , "sprightly"
      , "spring"
      , "sprinkle"
      , "spun"
      , "spurious"
      , "squiffy"
      , "squirm"
      , "staged"
      , "stagger"
      , "staggering"
      , "stampede"
      , "startle"
      , "stew"
      , "stir"
      , "storm"
      , "stormy"
      , "straighten"
      , "strange"
      , "strangely"
      , "strategy"
      , "stray"
      , "structured"
      , "struggle"
      , "struggling"
      , "stumble"
      , "stunt"
      , "stupid"
      , "stupidly"
      , "style"
      , "styled"
      , "stylistic"
      , "substance"
      , "substituted"
      , "suffer"
      , "supple"
      , "supply"
      , "surgery"
      , "surprise"
      , "surprising"
      , "suspect"
      , "swagger"
      , "sway"
      , "swilled"
      , "swim"
      , "swimming"
      , "swing"
      , "swirl"
      , "swirling"
      , "switch"
      , "switched"
      , "synthetic"
      , "tackle"
      , "tactics"
      , "tailor"
      , "tainted"
      , "take badly"
      , "take by surprise"
      , "take ill"
      , "take liberties"
      , "take wing"
      , "tamper"
      , "tangle"
      , "tawdry"
      , "tear"
      , "tease"
      , "teeter"
      , "teetering"
      , "temperamental"
      , "tempered"
      , "terrible"
      , "theatrical"
      , "thrash"
      , "thrashing about"
      , "threaten"
      , "throb"
      , "throw"
      , "tickle"
      , "tidy"
      , "tight"
      , "tinker"
      , "tipsy"
      , "tongue in cheek"
      , "topsy-turvy"
      , "torment"
      , "tormented"
      , "torn"
      , "tortuous"
      , "torture"
      , "tortured"
      , "toss"
      , "totter"
      , "touchy"
      , "tour"
      , "tousle"
      , "toy"
      , "tragic"
      , "train"
      , "trained"
      , "transfer"
      , "transform"
      , "transformed"
      , "transition"
      , "translate"
      , "translated"
      , "translation"
      , "transmute"
      , "transport"
      , "transpose"
      , "transposed"
      , "trash"
      , "trashed"
      , "travel"
      , "treachery"
      , "treat"
      , "treated"
      , "treatment"
      , "tremble"
      , "trepidation"
      , "trial"
      , "tribulation"
      , "trick"
      , "tricky"
      , "trim"
      , "trip"
      , "trouble"
      , "tumble"
      , "tumbling"
      , "tumult"
      , "tumultuous"
      , "turbulent"
      , "turmoil"
      , "turn"
      , "turning"
      , "tweak"
      , "tweaked"
      , "twirl"
      , "twist"
      , "twisted"
      , "twitch"
      , "type"
      , "ugly"
      , "unbalanced"
      , "unbridled"
      , "unbundle"
      , "uncanny"
      , "uncertain"
      , "uncivilised"
      , "uncontrolled"
      , "unconventional"
      , "uncultivated"
      , "uncured"
      , "undecided"
      , "undefined"
      , "undisciplined"
      , "undone"
      , "undulate"
      , "unearthly"
      , "unexpected"
      , "unfaithful"
      , "unfamiliar"
      , "unfettered"
      , "unfit"
      , "unfortunate"
      , "unfurl"
      , "ungainly"
      , "ungoverned"
      , "unhappy"
      , "unhealthy"
      , "unkempt"
      , "unlawful"
      , "unlikely"
      , "unnatural"
      , "unordered"
      , "unorthodox"
      , "unpredictable"
      , "unprepared"
      , "unravel"
      , "unravelling"
      , "unrecognisable"
      , "unrest"
      , "unrestrained"
      , "unrestricted"
      , "unruly"
      , "unscrambled"
      , "unscrupulous"
      , "unseemly"
      , "unsettle"
      , "unsettled"
      , "unsound"
      , "unstable"
      , "unsteady"
      , "untamed"
      , "untethered"
      , "untidy"
      , "untoward"
      , "untrained"
      , "untrue"
      , "untutored"
      , "unusual"
      , "unwieldy"
      , "up in the air"
      , "upheaval"
      , "uprise"
      , "uproar"
      , "upset"
      , "upset"
      , "vacillate"
      , "vacillating"
      , "vagrant"
      , "vague"
      , "vaguely"
      , "vandalized"
      , "variant"
      , "variation"
      , "variety"
      , "various"
      , "vary"
      , "veer"
      , "venture"
      , "versatile"
      , "version"
      , "vex"
      , "vibrate"
      , "vigorous"
      , "vigorously"
      , "violated"
      , "violent"
      , "violently"
      , "volatile"
      , "wacky"
      , "wag"
      , "wander"
      , "wandering"
      , "warp"
      , "warped"
      , "wasted"
      , "wave"
      , "waver"
      , "wayward"
      , "weaved"
      , "weird"
      , "weird"
      , "whack"
      , "whip up"
      , "whirl"
      , "whisk"
      , "wicked"
      , "wiggle"
      , "wild"
      , "wind"
      , "winding"
      , "wise"
      , "wobble"
      , "wonder"
      , "woolly"
      , "work"
      , "worked"
      , "worried"
      , "worry"
      , "wound"
      , "woven"
      , "wreck"
      , "wrecked"
      , "wrestle"
      , "wretched"
      , "wriggle"
      , "wring"
      , "writhe"
      , "writhing"
      , "wrong"
      , "wrought"
      , "yielding"
      , "zany"
      , "amended"
      ]

insertIndicators
  = S.fromList
      [ "aboard"
      , "aboard"
      , "accommodated"
      , "admitted to"
      , "admitted"
      , "admitted"
      , "amidst"
      , "amidst"
      , "appears in"
      , "besieged"
      , "besieged"
      , "between"
      , "boarding"
      , "boarding"
      , "boxed"
      , "boxed"
      , "breaking"
      , "breaking"
      , "bridged"
      , "bridged"
      , "buried in"
      , "buried"
      , "camouflages"
      , "carried by"
      , "caught"
      , "caught"
      , "characters"
      , "circled"
      , "circled"
      , "clutched"
      , "clutched"
      , "contained"
      , "contained"
      , "content"
      , "content"
      , "contents"
      , "contents"
      , "contents"
      , "contributing"
      , "covered"
      , "covered"
      , "dividing"
      , "dividing"
      , "dwells in"
      , "embraced"
      , "embraced"
      , "encircled"
      , "encircled"
      , "enfolded"
      , "enfolded"
      , "entering"
      , "entering"
      , "enveloped"
      , "enveloped"
      , "extract"
      , "filling"
      , "filling"
      , "flanked"
      , "flanked"
      , "furnishes"
      , "gives"
      , "grasped"
      , "grasped"
      , "harboured"
      , "harboured"
      , "held by"
      , "held"
      , "held"
      , "helping to make"
      , "hidden"
      , "hides"
      , "housed"
      , "housed"
      , "imbibed"
      , "in"
      , "in"
      , "included in"
      , "inherent in"
      , "inside"
      , "inside"
      , "inside"
      , "internal"
      , "internal"
      , "internal"
      , "interrupting"
      , "interrupting"
      , "intrinsic"
      , "introduced"
      , "introduced"
      , "involved in"
      , "inwardly"
      , "letters from"
      , "letters of"
      , "lining"
      , "lining"
      , "occupying"
      , "occupying"
      , "packing"
      , "packing"
      , "part"
      , "partial"
      , "parting"
      , "parting"
      , "partly"
      , "penetrating"
      , "penetrating"
      , "piercing"
      , "piercing"
      , "restrained"
      , "ringed"
      , "ringed"
      , "segment of"
      , "selection"
      , "separating"
      , "separating"
      , "sheltered"
      , "smuggling"
      , "some"
      , "somewhat"
      , "splitting"
      , "splitting"
      , "stuffing"
      , "stuffing"
      , "surrounded"
      , "surrounded"
      , "swallowed"
      , "taken in by"
      , "taken in"
      , "taken in"
      , "tucked in"
      , "tucked into"
      , "wearing"
      , "within"
      , "within"
      , "within"
      , "wrapped"
      ]

reverseInsertIndicators
  = S.fromList
      [ "about"
      , "accepts"
      , "admit"
      , "admits"
      , "admitting"
      , "around"
      , "besiege"
      , "besieges"
      , "besieging"
      , "box"
      , "boxes"
      , "boxing"
      , "bridge"
      , "bridges"
      , "bridging"
      , "capture"
      , "captured"
      , "captures"
      , "capturing"
      , "carries"
      , "carrying"
      , "catch"
      , "catches"
      , "catching"
      , "circle"
      , "circles"
      , "circling"
      , "clutch"
      , "clutches"
      , "clutching"
      , "concealed"
      , "concealing"
      , "conceals"
      , "contain"
      , "contained"
      , "containing"
      , "contains"
      , "cover"
      , "covering"
      , "covers"
      , "crossing"
      , "demonstrates"
      , "displaying"
      , "eclipsing"
      , "embrace"
      , "embraces"
      , "embracing"
      , "encircle"
      , "encircles"
      , "encircling"
      , "encloses"
      , "enfold"
      , "enfolding"
      , "enfolds"
      , "envelop"
      , "enveloping"
      , "envelops"
      , "exhibiting"
      , "external"
      , "featuring"
      , "flank"
      , "flanking"
      , "flanks"
      , "frame"
      , "framed"
      , "frames"
      , "framing"
      , "grasp"
      , "grasping"
      , "grasps"
      , "harbour"
      , "harbouring"
      , "harbours"
      , "has"
      , "hiding"
      , "hold"
      , "holding"
      , "holds"
      , "house"
      , "houses"
      , "housing"
      , "including"
      , "keeps"
      , "munching"
      , "on"
      , "outside"
      , "outwardly"
      , "overwhelms"
      , "protects"
      , "providing"
      , "receives"
      , "restrains"
      , "ring"
      , "ringing"
      , "rings"
      , "round"
      , "secreting"
      , "shelter"
      , "sheltered"
      , "sheltering"
      , "shelters"
      , "showing"
      , "stores"
      , "surround"
      , "surrounding"
      , "surroundings"
      , "surrounds"
      , "swallow"
      , "swallowed"
      , "swallowing"
      , "swallows"
      , "take in"
      , "takes in"
      , "taking in"
      , "to include"
      , "to pen"
      , "to secure"
      , "tours"
      , "traps"
      , "veiled"
      , "veiling"
      , "veils"
      , "wearing"
      , "withholds"
      , "without"
      , "wrap"
      , "wrapped"
      , "wrapping"
      , "wraps"
      ]

subIndicators
  = S.fromList
      [ "dropped"
      , "dropping"
      , "drops"
      , "has lost"
      , "having removed"
      , "letting slip"
      , "loses"
      , "lost"
      , "without"
      ]

reverseSubIndicators
  = S.fromList
      [ "from"
      , "leaving"
      , "out of"
      , "removed from"
      , "taken from"
      ]

hiddenIndicators
  = S.fromList
      [ "found in"
      , "from"
      , "inside"
      , "needed by"
      , "letters from"
      ]

reverseIndicators
  = S.fromList
      [ "about"
      , "after recovery"
      , "backwards"
      , "returned"
      , "returns"
      , "reversed"
      , "springs back"
      ]

firstsIndicators
  = S.fromList
      [ "all leaders in"
      , "all leaders"
      , "at first"
      , "first of"
      , "first"
      , "head of"
      , "head"
      , "leader"
      , "leaders"
      , "opening of"
      ]

lastsIndicators
  = S.fromList
      [ "end of"
      , "in the end"
      , "ultimate"
      ]

partsIndicators
  = S.fromList
      [ "almost"
      , "back of"
      , "bit of"
      , "dropping guts"
      , "front of"
      , "headless"
      , "mostly"
      , "nearly"
      , "nearly"
      , "part of"
      , "partly"
      , "tailless"
      ]

beforeIndicators
  = S.fromList
    [ "before" ]

afterIndicators
  = S.fromList
    [ "after" ]
