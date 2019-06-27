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
  case assemble input of
    Left err  -> print err
    Right asm -> mapM ppAssembler asm >> return ()

ppAssembler :: Assembler -> IO ()
ppAssembler (Labell str) = printf "@%s\n" str
ppAssembler (Inst inst) = case inst of
  Closure str -> printf "  closure %s\n" str
  Const int -> printf "  const %d\n" int
  Apply -> printf "  apply\n"
  Stop -> printf "  stop\n"
  Access int -> printf "  access %d\n" int
  Equal -> printf "  equal\n"
  Jump str -> printf "  jump %s\n" str
  Return -> printf "  return\n"
  Sub -> printf "  sub\n"
  Add -> printf "  add\n"
  

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
