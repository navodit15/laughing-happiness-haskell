-- writing a very simple parser (Part 2)
module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~" -- scheme indentifiers

{-
readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of 
  Left err -> "No match : " ++ show err 
  Right val -> "Found val" 
-}

-- adding whitespaces
spaces :: Parser()
spaces = skipMany1 space

{-
readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of  
  Left err -> "NO MATCH : " ++ show err
  Right val -> "Found value "
-}

data LispVal = Atom String 
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool


-- return lifts lispval into parser monad , return wraps and thus the parsestring takes no input and returns the inner value and thus has type lispval

parseString :: Parser LispVal
parseString = do 
    char '"' 
    x<- many (noneOf "\"")
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first : rest 
    return $ case atom of 
            "#t" -> Bool True
            "#f" -> Bool False
            _    -> Atom atom



main :: IO() 
main = do 
    (expr : _) <- getArgs
    putStrLn (readExpr expr)



