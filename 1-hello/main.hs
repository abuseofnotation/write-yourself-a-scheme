module Main where
 import System.Environment
 
 main :: IO ()
 main = do
     num1 <- getLine
     num2 <- getLine
     putStrLn (show((read num1) + (read num2)))
