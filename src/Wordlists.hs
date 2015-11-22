{-# LANGUAGE TemplateHaskell #-}

module Wordlists
  ( isInWordlist
  , isPrefixOfWord
  ) where

import Language.Haskell.TH

import qualified Data.Set as S

import Types

isInWordlist :: Word -> Bool
isInWordlist
  = flip S.member wordlist

wordlistLines :: [String]
wordlistLines
  = lines $(runIO (readFile "data/wordlist") >>= stringE)

wordlist :: S.Set Word
wordlist
  = S.fromList wordlistLines

isPrefixOfWord :: String -> Bool
isPrefixOfWord
  = flip S.member prefixWordlist

prefixes :: [a] -> [[a]]
prefixes xs
  = map (flip take xs) [n,n-1..1]
  where
    n = length xs

prefixWordlist :: S.Set String
prefixWordlist
  = S.fromList (concatMap prefixes wordlistLines)
