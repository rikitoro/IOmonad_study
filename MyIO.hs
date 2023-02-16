{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module MyIO (
  RealWorld,
  runtimeRealWorld,
  --
  MyIO (runMyIO),
  myPutStr,
  myGetChar,
  myGetLine
) where
  
-----

data RealWorld = RealWorld { _consoleOut :: String, _inputBuf :: String } deriving (Show)

runtimeRealWorld :: String -> String ->  RealWorld
runtimeRealWorld consoleOut inputBuf
  = RealWorld { _consoleOut = consoleOut, _inputBuf = inputBuf}

myPutStr' :: String -> RealWorld -> (RealWorld, ())
myPutStr' s w = (w { _consoleOut = _consoleOut w ++ s}, ())

myGetChar' :: RealWorld -> (RealWorld, Char)
myGetChar' w = (w { _inputBuf = tail $ _inputBuf w }, head $ _inputBuf w)


-----

newtype MyIO a = MyIO { runMyIO :: RealWorld -> (RealWorld, a) }

instance Functor MyIO where
  fmap :: (a -> b) -> MyIO a -> MyIO b
  fmap f (MyIO h) = MyIO $ \w ->
    let (nw, v)  = h w
    in  (nw, f v)

instance Applicative MyIO where
  pure :: a -> MyIO a
  pure a                = MyIO $ \w -> (w, a)
  (<*>) :: MyIO (a -> b) -> MyIO a -> MyIO b
  (MyIO f) <*> (MyIO x) = MyIO $ \w ->
    let (nw,  vf) = f w
        (nw', vx) = x nw
    in  (nw', vf vx)

instance Monad MyIO where
  (>>=) :: MyIO a -> (a -> MyIO b) -> MyIO b
  (MyIO h) >>= f  = MyIO $ \w ->
    let (nw, v)   = h w
        (MyIO g)  = f v
    in g nw

myPutStr :: String -> MyIO ()
myPutStr = MyIO . myPutStr'


myGetChar :: MyIO Char
myGetChar = MyIO myGetChar'


myGetLine :: MyIO String 
myGetLine = do
  c <- myGetChar
  if c == '\n'
    then return []
    else do
      cs <- myGetLine
      return (c:cs)
