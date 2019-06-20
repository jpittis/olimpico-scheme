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

eval :: Sexpr -> Env -> Maybe Value
eval expr env =
  case expr of
    Atom (Symbol s) -> Map.lookup s env
    Atom (Integer i) -> Just . Sexpr $ Atom (Integer i)
    List [(Atom (Symbol "if")), test, consq, alt] ->
      case eval test env of
        Just _ -> Just $ Sexpr consq
        Nothing -> Just $ Sexpr alt
    -- TODO: Not stateful so this wont work without a state monad or similar.
    -- List [(Atom (Symbol "define")), symbol, exp] ->
    --   Map.insert symbol (eval exp env) env
    List (fname : args) -> do
      (Func f) <- eval fname env
      args <- mapM (\arg -> eval arg env) args
      Just $ f args

hack :: Text -> Maybe Value
hack expr =
  case (parse sexpr "" expr) of
    Left _ -> Nothing
    Right parsed -> eval parsed stdenv
