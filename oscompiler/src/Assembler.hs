{-# LANGUAGE OverloadedStrings #-}
module Assembler
    ( assemble
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
    Labell String
  | Inst Inst
  deriving (Show)

data Inst =
    Closure String
  | Const Integer
  | Apply
  | Stop
  | Access Integer
  | Equal
  | Jump String
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
  return asm

labell :: Parser String
labell = char '@' >> some letterChar

inst :: Parser Inst
inst = Closure <$> (string "closure" >> space1 >> some letterChar)
  <|> Const <$> (string "const" >> L.decimal)
  <|> (string "apply" >> return Apply)
  <|> (string "stop" >> return Stop)
  <|> Access <$> (string "access" >> L.decimal)
  <|> (string "equal" >> return Equal)
  <|> Jump <$> (string "jump" >> some letterChar)
  <|> (string "return" >> return Return)
  <|> (string "sub" >> return Sub)
  <|> (string "add" >> return Add)

assemble :: Text -> Either Text [Assembler]
assemble input =
  case (parse progn "" input) of
    Left _ -> Left "Parse Error"
    Right p -> Right p
