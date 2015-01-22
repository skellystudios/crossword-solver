
module Wordlists (wordlist, outputWordlist, thesaurus, outputThesaurus, decodeThesaurus) where 

import Data.Set (Set,fromList) 
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Map as Map 
import Data.Array.IArray
import Data.Binary
import Data.ByteString.Lazy as BS

import System.IO.Unsafe


-- HEADLINE FUNCTIONS 


decodeThesaurus x = case (lookfor x) of 
  Nothing -> []
  Just x -> Prelude.map (\y -> keys!y) x

lookfor ::  String -> Maybe [Int]
lookfor x = Map.lookup x thesaurus

-- WORDLIST

readWordlist = do
    ls <- fmap Text.lines (Text.readFile "data/straight-wordlist")
    (return . Data.Set.fromList . stringRead . toString . Prelude.head) ls
    
tempWordlist = unsafePerformIO $ do readWordlist
outputWordlist = encodeFile "wordlist.bin" tempWordlist

readBytesWordlist :: IO (Data.Set.Set String)
readBytesWordlist = do
    return . decode =<< BS.readFile "wordlist.bin"
    
wordlist = unsafePerformIO $ do readBytesWordlist

-- THESAURUS

readThesaurus = do
    ls <- fmap Text.lines (Text.readFile "data/thesaurus-list")
    (return . Map.fromList . mapRead . toString . Prelude.head) ls
     
tempThesaurus = unsafePerformIO $ do readThesaurus
outputThesaurus = encodeFile "thesaurus.bin" tempThesaurus

readBytesThesaurus :: IO (Map.Map String [Int])
readBytesThesaurus = do
    return . decode =<< BS.readFile "thesaurus.bin"


thesaurus :: Map.Map String [Int]    
thesaurus = unsafePerformIO $ do readBytesThesaurus


-- KEYS

readKeys = do
    ls <- fmap Text.lines (Text.readFile "data/thesaurus-list-keys")
    (return . makeArray . stringRead . toString . Prelude.head) ls
     
tempKeys = unsafePerformIO $ do readKeys
outputKeys = encodeFile "keys.bin" tempKeys

readBytesKeys :: IO (Array Int String)
readBytesKeys = do
    return . decode =<< BS.readFile "keys.bin"
    
keys = unsafePerformIO $ do readBytesKeys



makeArray :: [String] -> Array Int String
makeArray xs = listArray (0, (Prelude.length xs) - 1) xs



toString = Text.unpack

-- do n <- readFile "data.dat" ; print n

-- 
mapRead :: String -> [(String, [Int])]
mapRead = read

stringRead :: String -> [String]
stringRead = read
