module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Parse.Parse
import Eval.Eval

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right v -> "Found value" ++ (show  (eval v))
 
main :: IO ()
--main = getLine >>= (liftM . readExpr)
main = do
  expr <-getLine
  print (readExpr expr)
