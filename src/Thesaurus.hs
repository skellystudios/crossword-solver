{-# LANGUAGE TemplateHaskell #-}

module Thesaurus
  ( thesaurusSynonyms
  ) where

import Language.Haskell.TH
import System.IO.Unsafe

import qualified Data.HashMap as H
import Data.Char
import System.Directory
import Debug.Trace

import Types
import Memoize

thesaurusSynonyms :: Phrase -> [Phrase]
thesaurusSynonyms phr
  = H.findWithDefault [] phr (phraseThesaurus phr)
  -- = H.findWithDefault [] phr thesaurus

-- thesaurusString :: String
-- thesaurusString
--   = $(runIO (readFile "../data/thesaurus.small") >>= stringE)
--
-- thesaurus :: H.Map Phrase [Phrase]
-- thesaurus
--   = makeHashMap thesaurusString

makeHashMap :: String -> H.Map Phrase [Phrase]
makeHashMap string = H.fromList (map read (lines string))

phraseThesaurus = memoize phraseThesaurusUnMemoized

phraseThesaurusUnMemoized :: Phrase -> H.Map Phrase [Phrase]
phraseThesaurusUnMemoized phr =
  let path = if null phr then "" else "../data/words/" ++ [Data.Char.toUpper( head phr) ] ++ "/" ++ phr
  in
    let exists = trace "rofl" unsafePerformIO .doesFileExist $ path
    in
      let string = if exists then unsafePerformIO . readFile $ path else ""
      in makeHashMap string
