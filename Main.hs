module Main where
import MyIO
    ( MyIO, runMyIO, myPutStr, myGetChar, myGetLine, runtimeRealWorld, RealWorld ) 

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
  myPutStr ['<', c1, '>']
  myPutStr ['<', c2, '>']

-- > runMyIO helloworldTest world 
-- ((),
-- Console : hogefuga HelloWorld<f><o>
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