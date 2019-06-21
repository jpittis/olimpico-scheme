{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as Text (getLine, putStr)
import System.IO (hFlush, stdout)
import Text.Megaparsec (parseTest)

import Lib
import Interpreter

main :: IO ()
main = repl

repl :: IO ()
repl = do
  Text.putStr "osi> "
  hFlush stdout
  line <- Text.getLine
  case hack line of
    Right (Func  _) -> print "Some Func"
    Right (Sexpr s) -> print s
    Left err        -> print err
  repl
