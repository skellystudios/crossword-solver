module Synonyms
  ( synonyms
  ) where

import Abbreviations
import Thesaurus
import Types

synonyms :: Phrase -> [Phrase]
synonyms phr
  = thesaurusSynonyms phr ++ abbreviations phr
