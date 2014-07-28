module Indicators where


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


isConsIndicator ["on"] = True
isConsIndicator ["over"] = True
isConsIndicator _ = False

isInsertionWord ["in"] = True
isInsertionWord ["involved", "in"] = True
isInsertionWord ["admitted", "to"] = True
isInsertionWord ["taken", "in", "by"] = True
isInsertionWord _ = False

isReverseInsertionWord ["over"] = True
isReverseInsertionWord ["crossing"] = True
isReverseInsertionWord ["contains"] = True
isReverseInsertionWord ["to", "secure"] = True
isReverseInsertionWord ["around"] = True
isReverseInsertionWord ["about"] = True
isReverseInsertionWord ["carrying"] = True
isReverseInsertionWord _ = False

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

isPartialIndicator ["mostly"] = True
isPartialIndicator ["almost"] = True
isPartialIndicator _ = False

isSubtractionWord ["leaving"] = True
isSubtractionWord ["without"] = True
isSubtractionWord ["letting","slip"] = True
isSubtractionWord _ = False
