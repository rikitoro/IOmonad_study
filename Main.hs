module Main where
import MyIO ( 
  MyIO, 
  myPutChar, myPutStr, myGetChar, myGetLine, 
  World, mkWorld ) 

-- initial World
world :: World
world = mkWorld "hogefuga " "bowwow\nmeow"

-- > world
-- World {_consoleOut = "hogefuga ", _inputBuffer = "bowwow\nmeow"}


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
-- ((),World {_consoleOut = "hogefuga HelloWorldob", _inputBuffer = "wwow\nmeow"})

-- test 2
myGetLineTest :: MyIO ()
myGetLineTest = do
  s <- myGetLine
  myPutStr s

-- > runMyIO myGetLineTest world 
-- ((),World {_consoleOut = "hogefuga bowwow", _inputBuffer = "meow"})


main :: IO ()
main = undefined