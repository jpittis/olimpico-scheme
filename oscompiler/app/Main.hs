{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as Text (getLine, putStr)
import System.IO (hFlush, stdout)
import Text.Megaparsec (parseTest)

import Lib
import Interpreter

main :: IO ()
main = repl stdenv

repl :: Env -> IO ()
repl env = do
  Text.putStr "osi> "
  hFlush stdout
  line <- Text.getLine
  let (result, env2) = hack env line
  case result of
    Right value -> print value
    Left err -> print err
  repl env2
