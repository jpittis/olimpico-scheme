{-# LANGUAGE OverloadedStrings #-}
module Parser
    ( exprs
    , sexpr
    , Sexpr(..)
    , Atom(..)
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as Text (pack)

type Parser = Parsec Void Text

data Sexpr =
    List [Sexpr]
  | Atom Atom
  deriving (Show, Read, Eq)

data Atom  =
    Integer Int
  | String  Text
  | Symbol  Text
  deriving (Show, Read, Eq)

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

atom :: Parser Atom
atom =
      Integer <$> L.decimal
  <|> Symbol . Text.pack <$> some (letterChar <|> symbolChar <|> char '-')
  <|> String <$> quotedString

quotedString :: Parser Text
quotedString = Text.pack <$> quoted (some letterChar)
  where
    quoted :: Parser a -> Parser a
    quoted = between (char '"') (char '"')

parens :: Parser a -> Parser a
parens = between (lexemeChar '(') (lexemeChar ')')
  where
    lexemeChar :: Char -> Parser Char
    lexemeChar = lexeme . char

sexpr :: Parser Sexpr
sexpr = List <$> parens (many (sexpr <|> Atom <$> lexeme atom))

exprs :: Parser [Sexpr]
exprs = lexeme $ many sexpr
