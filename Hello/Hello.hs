module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Parse.Parse
import Eval.Eval

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of

    --report parse errors
    Left err -> "Parse error: " ++ show err
    Right v -> case eval v of 
    --report runtime errors
        Left err -> "Runtime error: " ++ show err
        Right val -> ">" ++ show val
 
main :: IO ()
--main = getLine >>= (liftM . readExpr)
main = do
  expr <-getLine
  print (readExpr expr)
