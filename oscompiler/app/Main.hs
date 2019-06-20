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
    Just (Func _)  -> print "some func"
    Just (Sexpr s) -> print s
    Nothing        -> print "error"
  repl
