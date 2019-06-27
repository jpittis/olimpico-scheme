{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as Text (getLine, putStr, readFile)
import System.IO (hFlush, stdout)
import Text.Megaparsec (parseTest)
import System.CPUTime
import Text.Printf
import System.Environment

import Parser
import Interpreter
import Assembler

main :: IO ()
-- main = repl stdenv
main = assembler

assembler :: IO ()
assembler = do
  [filename] <- getArgs
  input <- Text.readFile filename
  let asm = (assemble input)
  print asm
  return ()

repl :: Env -> IO ()
repl env = do
  Text.putStr "osi> "
  hFlush stdout
  line <- Text.getLine
  start <- getCPUTime
  let (result, env2) = hack env line
  case result of
    Right value -> print value
    Left err    -> print err
  end  <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  printf "Duration: %0.3f sec\n" (diff :: Double)
  repl env2
