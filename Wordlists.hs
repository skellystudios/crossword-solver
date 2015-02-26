{-# LANGUAGE TemplateHaskell #-}

module Wordlists
  ( isInWordlist
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Data.Set as S

import Types

isInWordlist :: Word -> Bool
isInWordlist
  = flip S.member wordlist

wordlistString :: String
wordlistString
  = $(runIO (readFile "data/wordlist") >>= stringE)

wordlist :: S.Set Word
wordlist
  = S.fromList (lines wordlistString)
