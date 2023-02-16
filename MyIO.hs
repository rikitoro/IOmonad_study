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

myPutStr' :: String -> RealWorld -> ((), RealWorld)
myPutStr' s w = ((), w { _consoleOut = _consoleOut w ++ s})

myGetChar' :: RealWorld -> (Char, RealWorld)
myGetChar' w = (head $ _inputBuf w, w { _inputBuf = tail $ _inputBuf w })


-----

newtype MyIO a = MyIO { runMyIO :: RealWorld -> (a, RealWorld) }

instance Functor MyIO where
  fmap :: (a -> b) -> MyIO a -> MyIO b
  fmap f (MyIO h) = MyIO $ \w ->
    let (v, nw)  = h w
    in  (f v, nw)

instance Applicative MyIO where
  pure :: a -> MyIO a
  pure a                = MyIO $ \w -> (a, w)
  (<*>) :: MyIO (a -> b) -> MyIO a -> MyIO b
  (MyIO f) <*> (MyIO x) = MyIO $ \w ->
    let (vf,  nw) = f w
        (vx, nw') = x nw
    in  (vf vx, nw')

instance Monad MyIO where
  (>>=) :: MyIO a -> (a -> MyIO b) -> MyIO b
  (MyIO h) >>= f  = MyIO $ \w ->
    let (v, nw)   = h w
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
