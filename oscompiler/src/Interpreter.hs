{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Interpreter
    ( hack
    , Value(..)
    , Env
    , stdenv
    ) where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text as Text (unpack)
import Data.Text (Text)
import Text.Megaparsec (parse)
import Data.Either.Combinators (maybeToRight)
import Control.Monad.State

import Lib (Sexpr(..), Atom(..), sexpr, exprs)

type Func = [Value] -> State Env Value

data Env = Env
  { eLookup :: Map Text Value
  , eOuter  :: Maybe Env
  }

stdenv :: Env
stdenv =
  Env { eLookup = lookup, eOuter = Nothing }
  where
    lookup :: Map Text Value
    lookup =
      Map.fromList
        [ ("max", Func (\case [Sexpr (Atom (Integer a)), Sexpr (Atom (Integer b))] ->
                                return .Sexpr . Atom . Integer $ max a b))
        , ("min", Func (\case [Sexpr (Atom (Integer a)), Sexpr (Atom (Integer b))] ->
                                return . Sexpr. Atom . Integer $ min a b))
        , ("begin", Func (\case args -> return $ last args))
        ]

find :: Text -> Env -> Maybe Value
find s env =
  case Map.lookup s (eLookup env) of
    Just val -> Just val
    Nothing  -> eOuter env >>= find s

data Value = Func Func | Sexpr Sexpr | Nil

instance Show Value where
  show (Sexpr s) = show s
  show Nil       = "Nil"
  show (Func _)  = "Func"

type Result = Either Text Value

eval :: Sexpr -> State Env Result
eval expr =
  case expr of
    Atom (Symbol s) ->
      maybeToRight "Unknown Symbol" <$> gets (find s)
    Atom atom ->
      return . Right . Sexpr . Atom $ atom
    List [(Atom (Symbol "quote")), arg] ->
      return . Right . Sexpr $ arg
    List [(Atom (Symbol "if")), test, consq, alt] ->
      eval test >>= \case
        Right s -> return . Right $ if truthy s then Sexpr consq else Sexpr alt
        Left err -> return $ Left err
    List [(Atom (Symbol "define")), (Atom (Symbol symbol)), e] -> do
      eval e >>= \case
        Left err -> return $ Left err
        Right val -> do
          modify $ \env -> env { eLookup = Map.insert symbol val (eLookup env) }
          return $ Right val
    List [(Atom (Symbol "set!")), (Atom (Symbol symbol)), e] ->
      eval e >>= \case
        Left err -> return $ Left err
        Right val -> do
          -- TODO: set! should only edit existing environment or top level
          -- doing a recursive modify on Env sounds like a pain though
          modify $ \env -> env { eLookup = Map.insert symbol val (eLookup env) }
          return $ Right val
    List [(Atom (Symbol "lambda")), (List params), body] ->
      return . Right . Func $ func params body
    List (fname : args) ->
      eval fname >>= \case
        Left err -> return $ Left err
        Right (Func f) -> do
          fargs <- mapM eval args
          case sequence fargs of
            Left err -> return $ Left err
            Right lol -> Right <$> f lol
        Right _ -> return $ Left "Can't apply non function."

func :: [Sexpr] -> Sexpr -> Func
func params body =
  \args -> case (evalState (eval body) stdenv) of
    Left err  -> error (Text.unpack err)
    Right val -> return $ val
  -- TODO
  -- where
  --   buldenv :: [Sexpr] -> [Sexpr] -> Env -> Env
  --   buildenv params args env =

truthy :: Value -> Bool
truthy _ = True

hack :: Env -> Text -> (Result, Env)
hack env expr =
  case (parse sexpr "" expr) of
    Left _ -> (Left "Parse Error", env)
    Right parsed -> runState (eval parsed) env
