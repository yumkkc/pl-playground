
module Desc where

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec
import Data.Either

type Symbol = String

data SExp = Symbol Symbol | SExpList SList
  deriving (Eq)

newtype SList = SList [SExp]
  deriving (Eq)


printSExp :: SExp -> String  
printSExp (Symbol x)       = show x ++ " "
printSExp (SExpList slist) = show slist

printSList :: SList -> String
printSList (SList xs) = " ( " ++ concatMap printSExp xs ++ " ) "

instance Show SExp where
  show = printSExp

instance Show SList where  
  show  = printSList


parseSymbol :: Parser SExp
parseSymbol = Symbol <$> many1 letter

parseSExpList :: Parser SExp
parseSExpList = SExpList <$> parseSList

parseSExp :: Parser SExp
parseSExp = parseSymbol <|> parseSExpList

parseSpace :: Parser String
parseSpace = many1 $ char ' '

parseSList :: Parser SList
parseSList = do
    _ <- char '('
    sexps <- sepBy parseSExp parseSpace
    _ <- char ')'
    return $ SList sexps


parseExp :: String -> SList
parseExp xs = fromRight (SList []) $ parse parseSList "bebba" xs


-- main :: IO ()
-- main = case parse parseSList "" "(a (a b c) b c)" of
--   Left  err -> print err
--   Right val -> print val