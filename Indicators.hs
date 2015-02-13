module Indicators where

import qualified Data.Set 
import Utils

isDefIndicator ["and"] = True
isDefIndicator ["in"] = True
isDefIndicator ["to", "get"] = True
isDefIndicator ["brings", "out"] = True
isDefIndicator ["to", "make", "a"] = True
isDefIndicator ["for"] = True
isDefIndicator ["is"] = True
isDefIndicator ["providing"] = True
isDefIndicator ["makes"] = True
isDefIndicator ["made", "by"] = True
isDefIndicator ["for", "a"] = True
isDefIndicator _ = False


isJuxtapositionIndicator ["on"] = True
isJuxtapositionIndicator ["next", "to"] = True
isJuxtapositionIndicator ["with"] = True
isJuxtapositionIndicator ["beside"] = True
isJuxtapositionIndicator ["above"] = True
isJuxtapositionIndicator ["to"] = True
isJuxtapositionIndicator ["over"] = True
isJuxtapositionIndicator _ = False


isInsertionIndicator :: [String] -> Bool
isInsertionIndicator xs = Data.Set.member (concatWithSpaces xs) insertionIndicators
insertionIndicators = Data.Set.fromList [
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
	"involved in",
	"admitted to",
	"taken in by"]

isReverseInsertionIndicator :: [String] -> Bool
isReverseInsertionIndicator xs = Data.Set.member (concatWithSpaces xs) reverseInsertionIndicators
reverseInsertionIndicators = Data.Set.fromList [
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
	"withholds",
	"wrapped",
	"crossing",
	"contains",
	"to secure",
	"about",
	"around",
	"carrying"]



isRIndicator ["returned"] = True
isRIndicator ["springs", "back"] = True
isRIndicator ["about"] = True
isRIndicator _ = False

isHWIndicator ["found","in"] = True
isHWIndicator ["needed","by"] = True
isHWIndicator ["from"] = True
--isHWIndicator ["in"] = True
isHWIndicator _ = False

isFLIndicator ["leader"] = True
isFLIndicator ["at", "first"] = True
isFLIndicator ["first"] = True
isFLIndicator ["head"] = True
isFLIndicator ["first", "of"] = True
isFLIndicator _ = False

isLLIndicator ["in", "the ", "end"] = True
isLLIndicator ["end", "of"] = True
isLLIndicator _ = False

isPartOfIndicator ["bit", "of"] = True
isPartOfIndicator ["part", "of"] = True
isPartOfIndicator ["mostly"] = True
isPartOfIndicator ["partly"] = True
isPartOfIndicator ["almost"] = True
isPartOfIndicator ["nearly"] = True
isPartOfIndicator ["tailless"] = True
isPartOfIndicator _ = False

isSubtractionIndicator ["leaving"] = True
isSubtractionIndicator ["without"] = True
isSubtractionIndicator ["letting","slip"] = True
isSubtractionIndicator _ = False
