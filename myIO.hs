{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

data MyRealWorld = MyRealWorld { _consoleOutput :: String, _inputBuffer :: String } deriving (Show)

newtype MyIO a = MyIO { runMyIO :: MyRealWorld -> (MyRealWorld, a) }


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

myPutStr' :: String -> MyRealWorld -> (MyRealWorld, ())
myPutStr' s w = (w { _consoleOutput = _consoleOutput w ++ s}, ())

myGetChar :: MyIO Char
myGetChar = MyIO myGetChar'

myGetChar' :: MyRealWorld -> (MyRealWorld, Char)
myGetChar' w = (w { _inputBuffer = tail $ _inputBuffer w }, head $ _inputBuffer w)

myGetLine :: MyIO String 
myGetLine = do
  c <- myGetChar
  if c == '\n'
    then return []
    else do
      cs <- myGetLine
      return (c:cs)


-----------------------------

-- initial World
runtimeWorld :: MyRealWorld
runtimeWorld
  = MyRealWorld {
      _consoleOutput = "hogefuga ",
      _inputBuffer = "foobar\nmeaw" }

-- test 1
helloworldTest :: MyIO ()
helloworldTest = do
  myPutStr "Hello"
  myPutStr "World"
  c1 <- myGetChar
  c2 <- myGetChar
  myPutStr ['<', c1, '>']
  myPutStr ['<', c2, '>']

-- > runMyIO helloworldTest  runtimeWorld 
-- (MyRealWorld {_consoleOutput = "hogefuga HelloWorld<f><o>", _inputBuffer = "obar\nmeaw"},())


-- test 2
myGetLineTest :: MyIO ()
myGetLineTest = do
  s <- myGetLine
  myPutStr $ "<" ++ s ++ ">"

-- > runMyIO myGetLineTest runtimeWorld 
-- (MyRealWorld {_consoleOutput = "hogefuga <foobar>", _inputBuffer = "meaw"},())