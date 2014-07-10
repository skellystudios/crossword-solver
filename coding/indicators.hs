module Indicators where


isDefIndicator ["in"] = True
isDefIndicator ["for"] = True
isDefIndicator ["is"] = True
isDefIndicator ["providing"] = True
isDefIndicator ["makes"] = True
isDefIndicator _ = False


isConsIndicator ["on"] = True
isConsIndicator _ = False

isInsertionWord ["in"] = True
isInsertionWord ["admitted to"] = True
isInsertionWord _ = False

isReverseInsertionWord ["crossing"] = True
isReverseInsertionWord ["contains"] = True
isReverseInsertionWord ["around"] = True
isReverseInsertionWord ["about"] = True
isReverseInsertionWord _ = False

isRIndicator ["returned"] = True
isRIndicator ["about"] = True
isRIndicator _ = False

isHWIndicator ["found","in"] = True
isHWIndicator ["needed","by"] = True
isHWIndicator ["from"] = True
isHWIndicator _ = False

isFLIndicator ["leader"] = True
isFLIndicator ["at", "first"] = True
isFLIndicator ["first"] = True
isFLIndicator ["head"] = True
isFLIndicator ["first", "of"] = True
isFLIndicator _ = False

isLLIndicator ["in", "the ", "end"] = True
isLLIndicator ["first", "of"] = True
isLLIndicator _ = False

isPartialIndicator ["mostly"] = True
isPartialIndicator ["almost"] = True
isPartialIndicator _ = False

isSubtractionWord ["without"] = True
isSubtractionWord _ = False
