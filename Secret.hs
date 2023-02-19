-- ref.
-- Tsding, What is IO monad?, https://www.youtube.com/watch?v=fCoQb-zqYDI

module Secret 
  ( World ( World )
  , printStr
  , readStr  
  ) where

import System.IO.Unsafe (unsafePerformIO)

data World = World deriving Show

printStr :: String -> World -> World 
printStr s !w = unsafePerformIO $ putStrLn s >> return w 

readStr :: World -> (String, World)
readStr !w = unsafePerformIO $ getLine >>= \s -> return (s, w)