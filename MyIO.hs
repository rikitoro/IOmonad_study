{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module MyIO where 
import Prelude hiding (IO)

{-----------------------------}
{-- Simulate the Real World --}
{-----------------------------}

-- (A)
import World ( World ( World ), _inputBuffer, _consoleOut, printStr, readStr ) 
initWorld :: World
initWorld = World { _inputBuffer = "Rikitoro\nmeow", _consoleOut = "HOGE> "}

-- or, (B)
{- 
import Secret ( World ( World ), printStr, readStr )
initWorld :: World
initWorld = World
-}

-- (A)
-- ghci> initWorld
-- World {_consoleOut = "HOGE> ", _inputBuffer = "Rikitoro\nmeow"}
-- (B)
-- ghci> initWorld 
-- World


whatIsYourPureName :: World -> World 
whatIsYourPureName w1 = w4 
  where 
    w2          = printStr "What is your name?" w1 
    (name, w3)  = readStr w2 
    w4          = printStr ("Hello " ++ name) w3 

-- (A)
-- ghci> whatIsYourPureName initWorld 
-- World {_consoleOut = "HOGE> What is your name?Hello Rikitoro", _inputBuffer = "meow"}
-- (B)
-- ghci> whatIsYourPureName initWorld 
-- What is your name?
-- Rikitoro
-- Hello Rikitoro
-- World


{----------------------}
{-- Hiding the World --}
{----------------------}

type WorldT a = World -> (a, World)

readStrT :: WorldT String
readStrT = readStr

printStrT :: String -> WorldT () 
printStrT str w = ((), printStr str w)

infixl 1 >>>=
(>>>=)  :: WorldT a         -- World -> (a, World)
        -> (a -> WorldT b)  -- a -> World -> (b, World)
        -> WorldT b         -- World -> (b, World)
wt >>>= f = uncurry f . wt


whatIsYourPureNameT :: WorldT ()
whatIsYourPureNameT =
  printStrT "What is your name?" >>>= \_ ->
  readStrT >>>= \name ->
  printStrT $ "Hello " ++ name

-- (A)
-- ghci> whatIsYourPureNameT initWorld 
-- ((),World {_consoleOut = "HOGE> What is your name?Hello Rikitoro", _inputBuffer = "meow"})
-- (B)
-- ghci> whatIsYourPureNameT initWorld 
-- ((),What is your name?
-- Rikitoro
-- Hello Rikitoro
-- World)


{----------------------------}
{-- Do-notation for WorldT --}
{----------------------------}

newtype IO a = IO {   -- WorldM,
  runIO :: WorldT a   -- asT in ref. [2]
}

instance Functor IO where 
  fmap :: (a -> b) -> IO a -> IO b
  fmap f (IO wt) = IO $ 
    wt >>>= \x ->
    runIO . pure $ f x
  -- fmap f (IO wt) = IO $ \w ->
  --   let (x, nw) = wt w 
  --   in  (f x, nw)

instance Applicative IO where 
  pure :: a -> IO a
  pure x = IO $ \w -> (x, w)
  (<*>) :: IO (a -> b) -> IO a -> IO b
  (IO wtf) <*> (IO wt) = IO $ 
    wtf >>>= \f ->
    wt  >>>= \x ->
    runIO . pure $ f x
  -- (IO wtf) <*> (IO wt) = IO $ \w ->
  --   let (f, w')   = wtf w 
  --       (x, w'')  = wt w'
  --   in  (f x, w'')
    
instance Monad IO where 
  (>>=) :: IO a -> (a -> IO b) -> IO b
  (IO wt) >>= f = IO $
    wt >>>= runIO . f
  -- (IO wt) >>= f = IO $ \w ->
  --   let (x, w')   = wt w 
  --       (IO wt')  = f x
  --   in  wt' w'

printStrM :: String -> IO ()
printStrM = IO . printStrT

readStrM :: IO String 
readStrM = IO readStrT


whatIsYourPureNameM :: IO ()
whatIsYourPureNameM = do 
  printStrM "What is your name?"
  name <- readStrM 
  printStrM ("Hello " ++ name)

-- (A)
-- ghci> runIO whatIsYourPureNameM initWorld 
-- ((),World {_consoleOut = "HOGE> What is your name?Hello Rikitoro", _inputBuffer = "meow"})
-- (B)
-- ghci> runIO whatIsYourPureNameM initWorld 
-- ((),What is your name?
-- Rikitoro
-- Hello Rikitoro
-- World)