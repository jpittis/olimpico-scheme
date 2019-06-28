{-# LANGUAGE OverloadedStrings #-}
module Assembler
    ( assemble
    , progn
    , Assembler(..)
    , Inst(..)
    , dumpAssembler
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as Text (pack, concat)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.ByteString.Lazy as ByteString (concat, writeFile)
import Text.Printf
import qualified Data.Binary as Binary

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

buildLabelMap :: [Assembler] -> Map Text Integer
buildLabelMap asm =
  let (map, _) = foldl insertIfLabel (Map.empty, 0) asm in
    map
  where
    insertIfLabel :: (Map Text Integer, Integer) -> Assembler -> (Map Text Integer, Integer)
    insertIfLabel (map, i) (Labell txt) = (Map.insert txt i map, i)
    insertIfLabel (map, i) (Inst Return) = (map, i + 1)
    insertIfLabel (map, i) (Inst Equal) = (map, i + 1)
    insertIfLabel (map, i) (Inst Add) = (map, i + 1)
    insertIfLabel (map, i) (Inst Sub) = (map, i + 1)
    insertIfLabel (map, i) (Inst Apply) = (map, i + 1)
    insertIfLabel (map, i) (Inst Stop) = (map, i + 1)
    insertIfLabel (map, i) (Inst _) = (map, i + 2)

dumpAssembler :: [Assembler] -> FilePath -> IO ()
dumpAssembler asm path = do
  let bytecode = concat $ map (toBytecode $ buildLabelMap asm) asm
  ByteString.writeFile path (ByteString.concat $ map (Binary.encode . (fromIntegral :: Integer -> Binary.Word64)) bytecode)
  where
    toBytecode :: Map Text Integer -> Assembler -> [Integer]
    toBytecode _ (Labell _) = []
    toBytecode _ (Inst Return)          = [0]
    toBytecode _ (Inst Equal)           = [1]
    toBytecode _ (Inst Add)             = [2]
    toBytecode _ (Inst Sub)             = [3]
    toBytecode _ (Inst Apply)           = [7]
    toBytecode _ (Inst Stop)            = [8]
    toBytecode map (Inst (Closure txt)) = [9,  lookupLabel map txt]
    toBytecode map (Inst (Jump txt))    = [10, lookupLabel map txt]
    toBytecode _ (Inst (Const int))     = [11, int]
    toBytecode _ (Inst (Access int))    = [12, int]
    lookupLabel :: Map Text Integer -> Text -> Integer
    lookupLabel map txt = case Map.lookup txt map of
      Just int -> int
      Nothing  -> error (show ("cant find label",  txt, map))
