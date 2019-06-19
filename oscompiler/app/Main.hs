{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import qualified Data.Text.IO as Text (getLine, putStr)
import System.IO (hFlush, stdout)
import Text.Megaparsec (parseTest)

main :: IO ()
main = repl

repl :: IO ()
repl = do
  Text.putStr "osi> "
  hFlush stdout
  line <- Text.getLine
  parseTest exprs line
  repl
