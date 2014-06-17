import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
    ls <- fmap Text.lines (Text.readFile "data.dat")
    (print . words .head) ls


-- do n <- readFile "data.dat" ; print n