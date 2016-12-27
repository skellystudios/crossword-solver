module Memoize
  ( memoize
  ) where

--- FROM http://stackoverflow.com/questions/141650/how-do-you-make-a-generic-memoize-function-in-haskell
import qualified Data.Map as Map
import Data.IORef
import System.IO.Unsafe

memoize :: Ord a => (a -> b) -> (a -> b)
memoize f = unsafePerformIO $ do
    r <- newIORef Map.empty
    return $ \ x -> unsafePerformIO $ do
        m <- readIORef r
        case Map.lookup x m of
            Just y  -> return y
            Nothing -> do
                    let y = f x
                    writeIORef r (Map.insert x y m)
                    return y
