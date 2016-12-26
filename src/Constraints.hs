module Constraints
  ( Constraints
  , noConstraints

  , prefix
  , withNoPrefix
  , withPrefix

  , withNoMin
  , withMin

  , withNoMax
  , withMax

  , phraseFitsMax
  , phraseFitsMin
  , phraseFitsMaxMin

  , minLength
  , maxLength

  , makeConstraints -- This is temporary, remove it when I work this stuff out
  , maxOfNonEmptyList
  ) where

import Data.Functor

import Types
import Synonyms

newtype Constraints
  = Constraints (Maybe String, Maybe Length, Maybe Length) -- Prefix, Min, Max
  deriving (Eq, Show)

makeConstraints :: Maybe String -> Maybe Length -> Maybe Length -> Constraints
makeConstraints s n m = Constraints (s, n, m)

noConstraints :: Constraints
noConstraints
  = Constraints (Nothing, Nothing, Nothing)

prefix :: Constraints -> Maybe String
prefix (Constraints (maybePrefix, _, _))
  = maybePrefix

withNoPrefix :: Constraints -> Constraints
withNoPrefix (Constraints (_, maybeMinL, maybeMaxL))
  = Constraints (Nothing, maybeMinL, maybeMaxL)

withPrefix :: (String -> String) -> Constraints -> Constraints
withPrefix f (Constraints (maybePrefix, maybeMinL, maybeMaxL))
  = Constraints (f <$> maybePrefix, maybeMinL, maybeMaxL)

withNoMin :: Constraints -> Constraints
withNoMin (Constraints (maybePrefix, _, maybeMaxL))
  = Constraints (maybePrefix, Nothing, maybeMaxL)

withMin :: (Length -> Length) -> Constraints -> Constraints
withMin f (Constraints (maybePrefix, maybeMinL, maybeMaxL))
  = Constraints (maybePrefix, f <$> maybeMinL, maybeMaxL)

withNoMax :: Constraints -> Constraints
withNoMax (Constraints (maybePrefix, maybeMinL, _))
  = Constraints (maybePrefix, maybeMinL, Nothing)

withMax :: (Length -> Length) -> Constraints -> Constraints
withMax f (Constraints (maybePrefix, maybeMinL, maybeMaxL))
  = Constraints (maybePrefix, maybeMinL, f <$> maybeMaxL)

phraseFitsMin :: Constraints -> Phrase -> Bool
phraseFitsMin (Constraints (_, Just min, _)) phr
  = length phr >= min
phraseFitsMin (Constraints (_, Nothing, _)) phr
  = True

phraseFitsMax :: Constraints -> Phrase -> Bool
phraseFitsMax (Constraints (_, _, Just max)) phr
  = length phr <= max
phraseFitsMax (Constraints (_, _, Nothing)) phr
  = True

phraseFitsMaxMin :: Constraints -> Phrase -> Bool
phraseFitsMaxMin cs phr = (phraseFitsMin cs phr) && (phraseFitsMax cs phr)

minLength, maxLength :: ParseTree -> Length

minLength NullC               = 0
minLength (IdentC w)          = length w
minLength (JuxtC _ pt1 pt2)   = minLength pt1 + minLength pt2
minLength (ConcatC pts)       = foldr (\pt l -> minLength pt + l) 0 pts
minLength (SynC _)            = 0
minLength (AnagC _ ws)        = length (concat ws)
minLength (InsertC _ pt1 pt2) = minLength pt1 + minLength pt2
minLength (SubC _ pt1 pt2)    = max (minLength pt1 - maxLength pt2) 1
minLength (HiddenC _ _)       = 2
minLength (RevC _ pt)         = minLength pt
minLength (FirstsC _ ws)      = length ws
minLength (LastsC _ ws)       = length ws
minLength (PartC _ pt)        = 1

maxLength NullC               = 0
maxLength (IdentC w)          = length w
maxLength (JuxtC _ pt1 pt2)   = maxLength pt1 + maxLength pt2
maxLength (ConcatC pts)       = foldr (\pt l -> maxLength pt + l) 0 pts
maxLength (SynC w)            = maxOfNonEmptyList . (map length) . synonyms $ w
maxLength (AnagC _ ws)        = length (concat ws)
maxLength (InsertC _ pt1 pt2) = maxLength pt1 + maxLength pt2
maxLength (SubC _ pt1 pt2)    = max (maxLength pt1 - minLength pt2) 1
maxLength (HiddenC _ ws)      = length (concat ws) - 2
maxLength (RevC _ pt)         = maxLength pt
maxLength (FirstsC _ ws)      = length ws
maxLength (LastsC _ ws)       = length ws
maxLength (PartC _ pt)        = maxLength pt - 0

maxOfNonEmptyList l = if null l then 0 else maximum l
