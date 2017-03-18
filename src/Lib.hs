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
spaces = oneOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> satisfy isAlpha  <*> zeroOrMore (satisfy isAlphaNum)
