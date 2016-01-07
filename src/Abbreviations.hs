{-# LANGUAGE TemplateHaskell #-}

module Abbreviations
  ( abbreviations
  ) where

import Language.Haskell.TH

import qualified Data.HashMap as H

import Types

abbreviations :: Phrase -> [Phrase]
abbreviations phr
  = H.findWithDefault [] phr abbreviations_

abbreviationsString :: String
abbreviationsString
  = $(runIO (readFile "../data/abbreviations") >>= stringE)

abbreviations_ :: H.Map Phrase [Phrase]
abbreviations_
  = H.fromList (map read (lines abbreviationsString))
