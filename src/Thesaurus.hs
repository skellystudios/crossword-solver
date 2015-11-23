{-# LANGUAGE TemplateHaskell #-}

module Thesaurus
  ( thesaurusSynonyms
  ) where

import Language.Haskell.TH

import qualified Data.HashMap as H

import Types

thesaurusSynonyms :: Phrase -> [Phrase]
thesaurusSynonyms phr
  = H.findWithDefault [] phr thesaurus

thesaurusString :: String
thesaurusString
  = $(runIO (readFile "../data/thesaurus") >>= stringE)

thesaurus :: H.Map Phrase [Phrase]
thesaurus
  = H.fromList (map read (lines thesaurusString))
