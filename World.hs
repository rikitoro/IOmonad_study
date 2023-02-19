module World
  ( World ( World ), _inputBuffer, _consoleOut
  , printStr, readStr
  ) where

data World = World 
  { _consoleOut   :: String
  , _inputBuffer  :: String 
  } deriving Show

printStr :: String -> World -> World
printStr str w = w { _consoleOut = _consoleOut w ++ str }

readStr :: World -> (String, World)
readStr w =
  let (str, left)   = break (== '\n') (_inputBuffer w)
      inputBuffer'  = tail left
  in (str, w { _inputBuffer = inputBuffer'} )
