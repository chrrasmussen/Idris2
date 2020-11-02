module MultiHelloWorld

import Data.List
import System
import System.Concurrency
import System.Info

futureHelloWorld : (Int, String) -> IO (Future ())
futureHelloWorld (us, n) = fork $ do
  if (os == "darwin") then sleep (us `div` 1000) else usleep us
  putStrLn $ "Hello " ++ n ++ "!"

simpleIO : IO (List ())
simpleIO = do
  futures <- traverse futureHelloWorld [(3000, "World"), (1000, "Bar"), (0, "Foo"), (2000, "Idris")]
  awaited <- traverse await futures
  pure awaited

constant : IO ()
constant = do
  a <- fork (pure "String") >>= await
  putStrLn a
