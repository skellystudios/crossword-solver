module Databases (wordlist, thesaurus, keys, malenames, femalenames) where 

import Debug.Trace
import Data.Text
import qualified Data.Set as Set
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Map as Map 
import Data.Array.IArray
import Data.Binary
import Data.ByteString.Lazy as BS

import System.IO.Unsafe

import Types
import Utilities

-- WORDLISTS

read_malenames = do
    ls <- fmap Text.lines (Text.readFile "data/malenames")
    (return . Set.fromList . string_read . toString . toLower . Prelude.head) ls
    
temp_malenames = unsafePerformIO $ do read_malenames
output_malenames = encodeFile "malenames.bin" temp_malenames

read_bytes_malenames :: IO (Set.Set String)
read_bytes_malenames = do
    return . decode =<< BS.readFile "malenames.bin"
    
malenames 
  = trace "Male names..." (unsafePerformIO $ do read_bytes_malenames)

read_femalenames = do
    ls <- fmap Text.lines (Text.readFile "data/femalenames")
    (return . Set.fromList . string_read . toString . toLower . Prelude.head) ls
    
temp_femalenames = unsafePerformIO $ do read_femalenames
output_femalenames = encodeFile "femalenames.bin" temp_femalenames

read_bytes_femalenames :: IO (Set.Set String)
read_bytes_femalenames = do
    return . decode =<< BS.readFile "femalenames.bin"
    
femalenames 
  = trace "Female names" (unsafePerformIO $ do read_bytes_femalenames)

read_wordlist = do
    ls <- fmap Text.lines (Text.readFile "data/straight-wordlist")
    (return . Set.fromList . string_read . toString . Prelude.head) ls
    
temp_wordlist = unsafePerformIO $ do read_wordlist
output_wordlist = encodeFile "wordlist.bin" temp_wordlist

read_bytes_wordlist :: IO (Set.Set String)
read_bytes_wordlist = do
    return . decode =<< BS.readFile "wordlist.bin"
    
wordlist 
  = trace "Word list..." (unsafePerformIO $ do read_bytes_wordlist)

-- THESAURUS

read_thesaurus = do
    ls <- fmap Text.lines (Text.readFile "data/thesaurus-list")
    (return . Map.fromList . map_read . toString . Prelude.head) ls
     
temp_thesaurus = unsafePerformIO $ do read_thesaurus
output_thesaurus = encodeFile "thesaurus.bin" temp_thesaurus

read_bytes_thesaurus :: IO (Map.Map String [Int])
read_bytes_thesaurus = do
    return . decode =<< BS.readFile "thesaurus.bin"

thesaurus :: Map.Map String [Int]    
thesaurus 
  = trace "Thesaurus..." (unsafePerformIO $ do read_bytes_thesaurus)

-- KEYS

read_keys = do
    ls <- fmap Text.lines (Text.readFile "data/thesaurus-list-keys")
    (return . makeArray . string_read . toString . Prelude.head) ls
     
temp_keys = unsafePerformIO $ do read_keys
output_keys = encodeFile "keys.bin" temp_keys

read_bytes_keys :: IO (Array Int String)
read_bytes_keys = do
    return . decode =<< BS.readFile "keys.bin"
    
keys 
  = trace "Keys..." (unsafePerformIO $ do read_bytes_keys)

-- Stuff...

makeArray :: [String] -> Array Int String
makeArray ss = listArray (0, (Prelude.length ss) - 1) ss

toString = Text.unpack

map_read :: String -> [(String, [Int])]
map_read = read

string_read :: String -> [String]
string_read = read

