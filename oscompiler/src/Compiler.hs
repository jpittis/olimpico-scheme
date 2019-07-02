{-# LANGUAGE OverloadedStrings #-}
module Compiler
    ( fibCompile
    ) where

import Data.Text (Text)
import Text.Megaparsec (parse)

import Assembler
import Parser

fibScheme :: Text
fibScheme = "(lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))"

fibSexpr :: Sexpr
fibSexpr = case parse sexpr "" fibScheme of
  Right result -> result
  Left err -> error (show err)

fibCompile :: [Inst]
fibCompile = compileSexpr fibSexpr

compileSexpr :: Sexpr -> [Inst]
compileSexpr ast = case ast of
    List [(Atom (Symbol "lambda")), (List params), body] -> compileLambda params body
    List [(Atom (Symbol "if")), test, consq, alt] -> compileIf test consq alt
    List ((Atom (Symbol fname)) : args) -> compileFunc fname args

compileLambda params body = []

compileIf test consq alt =
  let testBlock  = compileSexpr test
      consqBlock = compileSexpr consq
      altBlock   = compileSexpr alt in
  concat
    [ testBlock
    -- , [ Jump (length) ]
    , consqBlock
    , altBlock
    ]

compileFunc fname args =
  concat
    [ [ Closure fname ]
    , concatMap compileSexpr args
    , [ Apply ]
    ]
