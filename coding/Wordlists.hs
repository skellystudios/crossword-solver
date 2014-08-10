
module Wordlists (wordlist, output_wordlist, thesaurus, output_thesaurus) where 

import Data.Set (Set,fromList) 
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Map as Map 
import Data.Binary
import Data.ByteString.Lazy as BS

import System.IO.Unsafe

read_wordlist = do
    ls <- fmap Text.lines (Text.readFile "data/straight-wordlist")
    (return . Data.Set.fromList . string_read . toString . Prelude.head) ls
    
temp_wordlist = unsafePerformIO $ do read_wordlist
output_wordlist = encodeFile "wordlist.bin" temp_wordlist

read_thesaurus = do
    ls <- fmap Text.lines (Text.readFile "data/thesaurus-list")
    (return . Map.fromList . map_read . toString . Prelude.head) ls
     
     
temp_thesaurus = unsafePerformIO $ do read_thesaurus
output_thesaurus = encodeFile "thesaurus.bin" temp_thesaurus


read_bytes_thesaurus :: IO (Map.Map String [String])
read_bytes_thesaurus = do
    return . decode =<< BS.readFile "thesaurus.bin"
    
read_bytes_wordlist :: IO (Data.Set.Set String)
read_bytes_wordlist = do
    return . decode =<< BS.readFile "wordlist.bin"
    

thesaurus = unsafePerformIO $ do read_bytes_thesaurus
wordlist = unsafePerformIO $ do read_bytes_wordlist



toString = Text.unpack

-- do n <- readFile "data.dat" ; print n

-- 
map_read :: String -> [(String, [String])]
map_read = read

string_read :: String -> [String]
string_read = read
