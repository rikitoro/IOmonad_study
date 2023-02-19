{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module MyIO (
  World,
  mkWorld,
  --
  MyIO ( asT ),
  myPutChar,
  myPutStr,
  myGetChar,
  myGetLine
) where
  
-----

-- simulate the World
data World = World 
  { _consoleOut   :: String
  , _inputBuffer  :: String 
  } deriving Show

mkWorld :: String -> String -> World
mkWorld consoleOut inputBuffer
  = World { _consoleOut = consoleOut, _inputBuffer = inputBuffer }

myPutChar' :: Char -> World -> World
myPutChar' c w = w { _consoleOut = _consoleOut w ++ [c]}

myGetChar' :: World -> (Char, World)
myGetChar' w = (head $ _inputBuffer w, w { _inputBuffer = tail $ _inputBuffer w })

-----

-- Hiding the world
-- see https://www.youtube.com/watch?v=fCoQb-zqYDI
type WorldT a = World -> (a, World)

myPutCharT :: Char -> WorldT ()
myPutCharT c w = ((), myPutChar' c w)

myGetCharT :: WorldT Char
myGetCharT = myGetChar'

-- bind operator
infixl 1 >>>=
(>>>=)  :: WorldT a         -- World -> (a, World)
        -> (a -> WorldT b)  -- a -> World -> (b, World) 
        -> WorldT b         -- World -> (b, World)
wt >>>= f = uncurry f . wt

----

-- monad
newtype MyIO a = MyIO {
  asT :: WorldT a
}

instance Functor MyIO where
  fmap :: (a -> b) -> MyIO a -> MyIO b
  fmap f wt = MyIO $
    asT wt >>>= \x ->
    asT $ pure $ f x 

instance Applicative MyIO where
  pure :: a -> MyIO a
  pure a = MyIO $ \w -> (a, w)
  (<*>) :: MyIO (a -> b) -> MyIO a -> MyIO b
  wtf <*> wtx = MyIO $ 
    asT wtf >>>= \f ->
    asT wtx >>>= \x ->
    asT $ pure $ f x

instance Monad MyIO where
  (>>=) :: MyIO a -> (a -> MyIO b) -> MyIO b
  wt >>= f = MyIO $ asT wt >>>= asT . f

myPutChar :: Char -> MyIO ()
myPutChar = MyIO . myPutCharT

myPutStr :: String -> MyIO ()
myPutStr = mapM_ myPutChar  

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
