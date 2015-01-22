import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Map as Map 

import System.IO.Unsafe

readWordlist = do
    ls <- fmap Text.lines (Text.readFile "data/straight-wordlist")
    (return . stringRead . toString . head) ls
    
wordlist = unsafePerformIO $ do readWordlist

readThesaurus = do
    ls <- fmap Text.lines (Text.readFile "data/thesaurus-list")
    (return . Map.fromList . mapRead . toString . head) ls
    

thesaurus = unsafePerformIO $ do readThesaurus

toString = Text.unpack

-- do n <- readFile "data.dat" ; print n

-- 
mapRead :: String -> [(String, [String])]
mapRead = read

stringRead :: String -> [String]
stringRead = read
