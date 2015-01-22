
module Wordlists (wordlist, thesaurus, decodeThesaurus) where 

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

readBytesWordlist :: IO (Data.Set.Set String)
readBytesWordlist = do
    return . decode =<< BS.readFile "wordlist.bin"
    
wordlist = unsafePerformIO $ do readBytesWordlist

-- THESAURUS

readBytesThesaurus :: IO (Map.Map String [Int])
readBytesThesaurus = do
    return . decode =<< BS.readFile "thesaurus.bin"

thesaurus :: Map.Map String [Int]    
thesaurus = unsafePerformIO $ do readBytesThesaurus


-- KEYS

readBytesKeys :: IO (Array Int String)
readBytesKeys = do
    return . decode =<< BS.readFile "keys.bin"
    
keys = unsafePerformIO $ do readBytesKeys



----- HERE ARE THE FUNCTIONS THAT COMPILE THE BINARIES ----

createWordlistBinary = encodeFile "wordlist.bin" tempWordlist

readWordlist = do
    ls <- fmap Text.lines (Text.readFile "data/straight-wordlist")
    (return . Data.Set.fromList . stringRead . toString . Prelude.head) ls
    
tempWordlist = unsafePerformIO $ do readWordlist

--

createKeysBinary = encodeFile "keys.bin" tempKeys

readKeys = do
    ls <- fmap Text.lines (Text.readFile "data/thesaurus-list-keys")
    (return . makeArray . stringRead . toString . Prelude.head) ls
     
tempKeys = unsafePerformIO $ do readKeys

---

readThesaurus = do
    ls <- fmap Text.lines (Text.readFile "data/thesaurus-list")
    (return . Map.fromList . mapRead . toString . Prelude.head) ls
     
tempThesaurus = unsafePerformIO $ do readThesaurus
createThesaurusBinary = encodeFile "thesaurus.bin" tempThesaurus




makeArray :: [String] -> Array Int String
makeArray xs = listArray (0, (Prelude.length xs) - 1) xs

toString = Text.unpack
 
mapRead :: String -> [(String, [Int])]
mapRead = read

stringRead :: String -> [String]
stringRead = read
