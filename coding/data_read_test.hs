import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Map as Map 

import System.IO.Unsafe

read_wordlist = do
    ls <- fmap Text.lines (Text.readFile "data/straight-wordlist")
    (return . string_read . toString . head) ls
    
wordlist = unsafePerformIO $ do read_wordlist

read_thesaurus = do
    ls <- fmap Text.lines (Text.readFile "data/thesaurus-list")
    (return . Map.fromList . map_read . toString . head) ls
    

thesaurus = unsafePerformIO $ do read_thesaurus

toString = Text.unpack

-- do n <- readFile "data.dat" ; print n

-- 
map_read :: String -> [(String, [String])]
map_read = read

string_read :: String -> [String]
string_read = read
