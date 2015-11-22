module Lists
  ( partitions
  , atLeastTwo

  , twoSplitsOf
  , mirroredTwoSplitsOf

  , threeSplitsOf
  , mirroredThreeSplitsOf

  , withMirrors
  , withMirrors3
  ) where

import Data.Tuple

import Types

partitions :: [a] -> [[[a]]]
partitions []
  = [[]]

partitions (x : xs)
  = [[x] : ps | ps <- pss] ++ [(x : ys) : yss | (ys : yss) <- pss]
  where
    pss = partitions xs

atLeastTwo :: [a] -> Bool
atLeastTwo (_ : _ : _)
  = True

atLeastTwo _
  = False

twoSplitsOf :: [a] -> PairsOf [a]
twoSplitsOf (x : xs)
  = ([x], xs) : [(x : ys, zs) | (ys, zs) <- twoSplitsOf xs]

twoSplitsOf _
  = []

threeSplitsOf :: [a] -> TriplesOf [a]
threeSplitsOf (x : xs)
  = [([x], ys, zs) | (ys, zs) <- twoSplitsOf xs] ++
      [(x : ys, zs, ws) | (ys, zs, ws) <- threeSplitsOf xs]

threeSplitsOf _
  = []

withMirrors :: PairsOf a -> PairsOf a
withMirrors xs
  = xs ++ map swap xs

withMirrors3 :: TriplesOf a -> TriplesOf a
withMirrors3 xs
  = xs ++ [(c, b, a) | (a, b, c) <- xs]

mirroredTwoSplitsOf :: [a] -> PairsOf [a]
mirroredTwoSplitsOf
  = withMirrors . twoSplitsOf

mirroredThreeSplitsOf :: [a] -> TriplesOf [a]
mirroredThreeSplitsOf
  = withMirrors3 . threeSplitsOf
