{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Interpreter
    ( hack
    , Value(..)
    ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text)
import Text.Megaparsec (parse)
import Data.Either.Combinators (maybeToRight)

import Lib (Sexpr(..), Atom(..), sexpr)

type Func = [Value] -> Value

type Env = Map Text Value

stdenv :: Env
stdenv =
  Map.fromList
    [ ("max", Func (\case [Sexpr (Atom (Integer a)), Sexpr (Atom (Integer b))] ->
                            Sexpr . Atom . Integer $ max a b))
    , ("min", Func (\case [Sexpr (Atom (Integer a)), Sexpr (Atom (Integer b))] ->
                            Sexpr. Atom . Integer $ min a b))
    ]

data Value = Func Func | Sexpr Sexpr

type Result = Either Text Value

eval :: Env -> Sexpr -> Result
eval env expr =
  case expr of
    Atom (Symbol s) -> maybeToRight "Unknown Symbol" (Map.lookup s env)
    Atom (Integer i) -> Right . Sexpr $ Atom (Integer i)
    List [(Atom (Symbol "if")), test, consq, alt] ->
      case eval env test of
        Right s -> Right $ if truthy s then Sexpr consq else Sexpr alt
        Left err -> Left err
    -- TODO: Not stateful so this wont work without a state monad or similar.
    -- List [(Atom (Symbol "define")), symbol, exp] ->
    --   Map.insert symbol (eval exp env) env
    List (fname : args) ->
      case (eval env fname) of
        Left err -> Left err
        Right (Func f) -> f <$> mapM (eval env) args
        Right _ -> Left "Can't apply non function."

truthy :: Value -> Bool
truthy _ = True

hack :: Text -> Result
hack expr =
  case (parse sexpr "" expr) of
    Left _ -> Left "Parse Error"
    Right parsed -> eval stdenv parsed
