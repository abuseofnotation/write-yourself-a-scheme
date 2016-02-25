module Eval.Unpack where

import Parse.Parse

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
                           if null parsed 
                              then 0
                              else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

unpackBoolean:: LispVal -> Bool
unpackBoolean val = case val of 
                      Atom "#t" -> True
                      otherwise -> False

unpackString :: LispVal -> String
unpackString val = case val of 
                      String val -> val
                      otherwise -> show val
