module Main where
import MyIO ( 
  MyIO, runMyIO, 
  myPutChar, myPutStr, myGetChar, myGetLine, 
  RealWorld, runtimeRealWorld ) 

-- initial World
world :: RealWorld
world = runtimeRealWorld "hogefuga " "foobar\nmeaw"

-- > world
--
-- Console : hogefuga 
-- InputBuf: foobar
-- meaw


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
-- ((),
-- Console : hogefuga HelloWorldof
-- InputBuf: obar
-- meaw
-- )


-- test 2
myGetLineTest :: MyIO ()
myGetLineTest = do
  s <- myGetLine
  myPutStr $ "<" ++ s ++ ">"

-- > runMyIO myGetLineTest world 
-- ((),
-- Console : hogefuga <foobar>
-- InputBuf: meaw
-- )

main :: IO ()
main = undefined