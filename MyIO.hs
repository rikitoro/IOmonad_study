{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module MyIO where 


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

newtype MyIO a = MyIO { -- WorldM,
  runMyIO :: WorldT a   -- asT in ref. [2]
}

instance Functor MyIO where 
  fmap :: (a -> b) -> MyIO a -> MyIO b
  fmap f myio = MyIO $ 
    runMyIO myio >>>= \x ->
    runMyIO . pure $ f x

instance Applicative MyIO where 
  pure :: a -> MyIO a
  pure x = MyIO $ \w -> (x, w)
  (<*>) :: MyIO (a -> b) -> MyIO a -> MyIO b
  myiof <*> myio = MyIO $ 
    runMyIO myiof >>>= \f ->
    runMyIO myio  >>>= \x ->
    runMyIO . pure $ f x

instance Monad MyIO where 
  (>>=) :: MyIO a -> (a -> MyIO b) -> MyIO b
  myio >>= f = MyIO $
    runMyIO myio >>>= runMyIO . f

printStrM :: String -> MyIO ()
printStrM = MyIO . printStrT

readStrM :: MyIO String 
readStrM = MyIO readStrT


whatIsYourPureNameM :: MyIO ()
whatIsYourPureNameM = do 
  printStrM "What is your name?"
  name <- readStrM 
  printStrM ("Hello " ++ name)

-- (A)
-- ghci> runMyIO whatIsYourPureNameM initWorld 
-- ((),World {_consoleOut = "HOGE> What is your name?Hello Rikitoro", _inputBuffer = "meow"})
-- (B)
-- ghci> runMyIO whatIsYourPureNameM initWorld 
-- ((),What is your name?
-- Rikitoro
-- Hello Rikitoro
-- World)