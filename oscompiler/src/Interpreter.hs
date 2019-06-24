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
import qualified Data.Text as Text (unpack, pack)
import Data.Text (Text)
import Text.Megaparsec (parse)
import Data.Either.Combinators (maybeToRight)
import Control.Monad.State

import Parser (Sexpr(..), Atom(..), sexpr, exprs)

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
        , ("+", Func (\case [Sexpr (Atom (Integer a)), Sexpr (Atom (Integer b))] ->
                                return . Sexpr . Atom . Integer $ a + b))
        , ("-", Func (\case [Sexpr (Atom (Integer a)), Sexpr (Atom (Integer b))] ->
                                return . Sexpr . Atom . Integer $ a - b))
        ]

find :: Text -> Env -> Maybe Value
find s env =
  case Map.lookup s (eLookup env) of
    Just val -> Just val
    Nothing  -> eOuter env >>= find s

setbang :: Text -> Value -> Env -> Env
setbang s v env = do
  case Map.member s (eLookup env) of
    True  -> setdefine s v env
    False ->
      case eOuter env of
        Nothing -> setdefine s v env
        Just outer ->
          env { eOuter = Just $ setbang s v outer }

setdefine :: Text -> Value -> Env -> Env
setdefine s v env =
  env { eLookup = Map.insert s v (eLookup env) }

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
      maybeToRight (Text.pack . show $ ("unknown symbol", s)) <$> gets (find s)
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
          modify (setdefine symbol val)
          return $ Right val
    List [(Atom (Symbol "set")), (Atom (Symbol symbol)), e] ->
      eval e >>= \case
        Left err -> return $ Left err
        Right val -> do
          modify (setbang symbol val)
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
func params body = do
  \args -> do
    env <- get
    case evalState (eval body) (buildenv params args env)  of
      Left err -> error (Text.unpack err)
      Right val -> return $ val
  where
    buildenv :: [Sexpr] -> [Value] -> Env -> Env
    buildenv params args outer =
      Env { eLookup = buildlookup params args, eOuter = Just outer }
    buildlookup :: [Sexpr] -> [Value] -> Map Text Value
    buildlookup params args = Map.fromList . mapToEnv $ (zip params args)
    mapToEnv :: [(Sexpr, Value)] -> [(Text, Value)]
    mapToEnv = map mapToTxt
    mapToTxt :: (Sexpr, Value) -> (Text, Value)
    mapToTxt ((Atom (Symbol s)), v) = (s, v)

truthy :: Value -> Bool
truthy Nil = False
truthy _ = True

hack :: Env -> Text -> (Result, Env)
hack env expr =
  case (parse sexpr "" expr) of
    Left _ -> (Left "Parse Error", env)
    Right parsed -> runState (eval parsed) env
