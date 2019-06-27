{-# LANGUAGE OverloadedStrings #-}
module Assembler
    ( assemble
    , progn
    , Assembler(..)
    , Inst(..)
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as Text (pack)

type Parser = Parsec Void Text

data Assembler =
    Labell Text
  | Inst Inst
  deriving (Show)

data Inst =
    Closure Text
  | Const Integer
  | Apply
  | Stop
  | Access Integer
  | Equal
  | Jump Text
  | Return
  | Sub
  | Add
  deriving (Show)

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

progn :: Parser [Assembler]
progn = lexeme $ many assembler

assembler :: Parser Assembler
assembler = do
  asm <- Labell <$> labell <|> Inst <$> inst
  newline
  sc
  return asm

labell :: Parser Text
labell = char '@' >> alphaNumText

alphaNumText = Text.pack <$> some alphaNumChar

inst :: Parser Inst
inst = Closure <$> (string "closure" >> space1 >> alphaNumText)
  <|> Const <$> (string "const" >> space1 >> L.decimal)
  <|> (string "apply" >> return Apply)
  <|> (string "stop" >> return Stop)
  <|> Access <$> (string "access" >> space1 >> L.decimal)
  <|> (string "equal" >> return Equal)
  <|> Jump <$> (string "jump" >> space1 >> alphaNumText)
  <|> (string "return" >> return Return)
  <|> (string "sub" >> return Sub)
  <|> (string "add" >> return Add)

assemble :: Text -> Either Text [Assembler]
assemble input =
  case (parse progn "" input) of
    Left _ -> Left "Parse Error"
    Right p -> Right p
