{-# LANGUAGE FlexibleContexts, MultiWayIf #-}
module Eval.Eval where

import Parse.Parse
import Eval.Unpack
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


eval badForm = Left $ "Unrecognized form" ++ show badForm
--list
evalArgs :: [LispVal] -> Either String LispVal
evalArgs args = case find isLeft evaluated of
        Nothing -> Right $ List $ map unRight evaluated 
        Just error -> error
        where evaluated = map eval args

--A list of functions
functions :: [(String, [LispVal] -> LispVal)]
functions = [("+", numberToNumber (+)),
              ("-", numberToNumber (-)),
              ("*", numberToNumber (*)),
              ("/", numberToNumber div),
              ("mod", numberToNumber mod),
              ("quotient", numberToNumber quot),
              ("remainder", numberToNumber rem),
              ("=", numberToBoolean (==)),
              ("<", numberToBoolean (<)),
              (">", numberToBoolean (>)),
              ("/=", numberToBoolean (/=)),
              (">=", numberToBoolean (>=)),
              ("<=", numberToBoolean (<=)),
              ("&&", booleanToBoolean (&&)),
              ("||", booleanToBoolean (||)),
              ("string=?", stringToBoolean (==)),
              ("string<?", stringToBoolean (<)),
              ("string>?", stringToBoolean (>)),
              ("string<=?", stringToBoolean (<=)),
              ("string>=?", stringToBoolean (>=)),
              ("car", car),
              ("cdr", cdr)]
              --("cons", cons)]

car :: [LispVal] -> LispVal
car [List (x:xs)] = x
car [(DottedList (x:xs) tail)] = x

cdr [List (x:xs)] = List xs
cdr [(DottedList (x:[]) tail)] = tail
cdr [(DottedList (x:xs) tail)] = DottedList xs tail

--Converts a Haskell function to Lisp function
toLispFunction unpack pack func = \list -> pack $ func (map unpack list)

numberToNumber :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numberToNumber = toLispFunction unpackNum Number . foldl1

numberToBoolean = toLispFunction unpackNum Bool . applyBoolean

booleanToBoolean = toLispFunction unpackBoolean Bool . applyBoolean

stringToBoolean = toLispFunction unpackString Bool . applyBoolean

applyBoolean _ [] = False
applyBoolean _ [val] = False
applyBoolean op (val1:val2:xs) = if (op val1 val2)
                                    then if xs == []
                                        then True
                                        else applyBoolean op (val2:xs)
                                    else False

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

