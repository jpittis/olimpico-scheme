{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Interpreter
    ( hack
    , hacks
    , Value(..)
    ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text)
import Text.Megaparsec (parse)
import Data.Either.Combinators (maybeToRight)
import Control.Monad.State

import Lib (Sexpr(..), Atom(..), sexpr, exprs)

type Func = [Value] -> Value

type Env = Map Text Value

stdenv :: Env
stdenv =
  Map.fromList
    [ ("max", Func (\case [Sexpr (Atom (Integer a)), Sexpr (Atom (Integer b))] ->
                            Sexpr . Atom . Integer $ max a b))
    , ("min", Func (\case [Sexpr (Atom (Integer a)), Sexpr (Atom (Integer b))] ->
                            Sexpr. Atom . Integer $ min a b))
    , ("begin", Func (\case args -> last args))
    ]

data Value = Func Func | Sexpr Sexpr | Nil

type Result = Either Text Value

eval :: Sexpr -> State Env Result
eval expr =
  case expr of
    Atom (Symbol s) ->
      maybeToRight "Unknown Symbol" <$> gets (Map.lookup s)
    Atom (Integer i) ->
      return . Right . Sexpr $ Atom (Integer i)
    List [(Atom (Symbol "if")), test, consq, alt] ->
      eval test >>= \case
        Right s -> return . Right $ if truthy s then Sexpr consq else Sexpr alt
        Left err -> return $ Left err
    List [(Atom (Symbol "define")), (Atom (Symbol symbol)), e] -> do
      eval e >>= \case
        Left err -> return $ Left err
        Right val -> do
          modify $ Map.insert symbol val
          return . Right $ Nil
    List (fname : args) ->
      eval fname >>= \case
        Left err -> return $ Left err
        Right (Func f) -> do -- return (f <$> mapM eval args)
          fargs <- mapM eval args
          case sequence fargs of
            Left err -> return $ Left err
            Right lol -> return . Right $ f lol
        Right _ -> return $ Left "Can't apply non function."

truthy :: Value -> Bool
truthy _ = True

hack :: Text -> Result
hack expr =
  case (parse sexpr "" expr) of
    Left _ -> Left "Parse Error"
    Right parsed -> evalState (eval parsed) stdenv

hacks :: Text -> [Result]
hacks expr =
  case (parse exprs "" expr) of
    Left _ -> [Left "Parse Error"]
    Right parsed -> evalState (mapM eval parsed) stdenv
