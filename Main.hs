module Main where
import MyIO ( 
  MyIO, runMyIO, 
  myPutChar, myPutStr, myGetChar, myGetLine, 
  RealWorld, mkRealWorld ) 

-- initial World
world :: RealWorld
world = mkRealWorld "hogefuga " "bowwow\nmeow"

-- > world
-- RealWorld {_consoleOut = "hogefuga ", _inputBuffer = "bowwow\nmeow"}


-- test 1
helloworldTest :: MyIO ()
helloworldTest = do
  myPutStr "Hello"
  myPutStr "World"
  c1 <- myGetChar
  c2 <- myGetChar
  myPutChar c2
  myPutChar c1

-- > runMyIO helloworldTest world 
-- ((),RealWorld {_consoleOut = "hogefuga HelloWorldob", _inputBuffer = "wwow\nmeow"})

-- test 2
myGetLineTest :: MyIO ()
myGetLineTest = do
  s <- myGetLine
  myPutStr s

-- > runMyIO myGetLineTest world 
-- ((),RealWorld {_consoleOut = "hogefuga bowwow", _inputBuffer = "meow"})


main :: IO ()
main = undefined