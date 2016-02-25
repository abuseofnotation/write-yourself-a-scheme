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
        Right val -> show val
 
main :: IO ()
--main = getLine >>= (liftM . readExpr)
main = repl

repl = do
    putStr "scheme> "
    expr <- getLine
    if (expr /= ":q")
      then putStr ((readExpr expr) ++ "\n" )>>  repl
      else return ()
