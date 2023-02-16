module Main where
import MyIO
    ( MyIO, runMyIO, myPutStr, myGetChar, myGetLine, runtimeRealWorld, RealWorld ) 

-- initial World
world :: RealWorld
world = runtimeRealWorld "hogefuga " "foobar\nmeaw"

-- > world
-- > RealWorld {_consoleOut = "hogefuga ", _inputBuf = "foobar\nmeaw"}


-- test 1
helloworldTest :: MyIO ()
helloworldTest = do
  myPutStr "Hello"
  myPutStr "World"
  c1 <- myGetChar
  c2 <- myGetChar
  myPutStr ['<', c1, '>']
  myPutStr ['<', c2, '>']

-- > runMyIO helloworldTest world
-- (MyRealWorld {_consoleOutput = "hogefuga HelloWorld<f><o>", _inputBuffer = "obar\nmeaw"},())


-- test 2
myGetLineTest :: MyIO ()
myGetLineTest = do
  s <- myGetLine
  myPutStr $ "<" ++ s ++ ">"

-- > runMyIO myGetLineTest world 
-- (MyRealWorld {_consoleOutput = "hogefuga <foobar>", _inputBuffer = "meaw"},())


main :: IO ()
main = undefined