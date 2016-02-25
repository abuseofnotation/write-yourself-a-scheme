module Eval.Environment where

import Parse.Parse 

type Function = Environment -> (Environment -> Environment) -> [LispVal] -> (Environment, LispVal)
)

newtype Environment = Environment (Maybe [(String, Function)])



