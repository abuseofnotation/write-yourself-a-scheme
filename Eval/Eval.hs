{-# LANGUAGE FlexibleContexts, MultiWayIf #-}
module Eval.Eval where

import Parse.Parse
import Data.List
import Data.Either


eval :: LispVal -> Either String LispVal
eval val@(String _) = Right val
eval val@(Number _) = Right val
eval val@(Bool _) = Right val

--list literal
eval (List [Atom "quote", val]) = Right val

--function application
eval (List ((Atom f):args)) = (evalArgs args) >>= apply f 

--list
evalArgs :: [LispVal] -> Either String LispVal
evalArgs args = case find isLeft evaluated of
        Nothing -> Right $ List $ map unRight evaluated 
        Just error -> error
        where evaluated = map eval args

--A list of functions
functions :: [(String, [LispVal] -> LispVal)]
functions = [
  ("+", \args -> Number $ sum (toNumbers args))]

--Evaluating function application
apply :: String -> LispVal -> Either String LispVal
apply name (List args) = case f of 
                    Nothing -> Left $ "Undefined function: " ++ name
                    Just func ->  Right $ func args
                  where f = lookup name functions 


unRight :: Either a b -> b
unRight (Right b) = b

toNumber :: LispVal -> Integer
toNumber (Number x) = x

toNumbers :: [LispVal] -> [Integer]
toNumbers = fmap toNumber

