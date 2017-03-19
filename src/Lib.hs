{-# LANGUAGE OverloadedStrings #-}

module Lib where

import AParser
import Control.Applicative
import Data.Char

someFunc :: IO ()
someFunc = putStrLn "someFunc"

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

type Ident = String

data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseAtom :: Parser Atom
parseAtom =  spaces *> (I <$> ident <|> N <$> posInt) 

parseSExpr :: Parser SExpr
parseSExpr =  (spaces *> char '(' *> (Comb <$> oneOrMore parseSExpr) <* spaces <* char ')' <* spaces) <|> (A <$> parseAtom) 
{-parseSExpr =  (char '(' *> (Comb <$> oneOrMore parseSExpr) <* char ')') <|> (A <$> parseAtom) -}
