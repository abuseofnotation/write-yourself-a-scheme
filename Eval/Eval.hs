{-# LANGUAGE FlexibleContexts, MultiWayIf #-}
module Eval.Eval where

import Parse.Parse
import Data.List


eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
--list literal
eval (List [Atom "quote", val]) = val
--function application
eval (List ((Atom f):args)) = apply f (map eval args)


apply :: String -> [LispVal] -> LispVal

apply "+" list = Number $ sum (toNumbers list)

toNumber :: LispVal -> Integer
toNumber (Number x) = x

toNumbers :: [LispVal] -> [Integer]
toNumbers = fmap toNumber

