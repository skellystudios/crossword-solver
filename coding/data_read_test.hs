import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
    ls <- fmap Text.lines (Text.readFile "filename.txt")
    head(ls)

-- do n <- readFile "data.dat" ; print n