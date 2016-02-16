module Parse.Parse where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad


data LispVal = Atom String
             | Nil
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Character Char
             | Bool Bool deriving (Show)

parseExpr :: Parser LispVal
parseExpr = parseChar
         <|> parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                --x <- try parseList <|> parseDottedList
                x <- parseAnyList
                char ')'
                return x


trySpaces :: Parser ()
trySpaces = spaces <|> return ()

--3. Instead of using the try combinator, left-factor the grammar so that the common subsequence is its own parser. 

parseAnyList :: Parser LispVal
parseAnyList  = do 
    list <- sepBy parseExpr space
    trySpaces
    tail <- (char '.' >> spaces >> parseExpr) <|> return Nil
    return $ case tail of 
        Nil -> List list
        otherwise -> DottedList list tail

parseList :: Parser LispVal
parseList = sepBy parseExpr space >>= \a -> return $ List a

  

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- sepBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail 

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

escapedChars :: Parser Char
escapedChars = do 
              --TODO Why does this work with single quotes, but not with double
              char '\\'
              x <- oneOf "\\\""
              return x

-- 2. Change parseString so that \" gives a literal quote character instead of terminating the string. 

chars :: Parser Char
chars = escapedChars <|> noneOf "\""

parseChar :: Parser LispVal
parseChar  = do
              string "\\#"
              ch <- chars
              return $ Character ch


parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (chars)
                char '"'
                return $ String x
 

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right v -> "Found value" ++ show v


parseNumber :: Parser LispVal
--parseNumber = liftM (Number . read) $ many1 digit

--1. Rewrite parseNumber, without liftM, using:

-- 1.explicit sequencing with the >>= operator
parseNumber  = many1 digit >>= \val -> return $ Number (read val)

--TODO find why this does not work
--parseNumber str = many1 digit $ str >>= \val -> return $ Number (read val)

-- 2.do-notation
--parseNumber = do 
--                val <- many1 digit 
--                return $ Number (read val)

