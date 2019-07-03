{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Compiler
    ( fibCompile
    ) where

import Data.Text (Text)
import Text.Megaparsec (parse)
import Control.Monad.Reader (Reader, runReader, asks, local)
import Data.Map (Map)
import qualified Data.Map as Map (empty, lookup, fromList, union)

import Parser

data ClosureIdent =
    ClosureOffset Int
  | ClosureName Text
  deriving (Show)

data Inst =
    Closure ClosureIdent
  | Const Int
  | Apply
  | Stop
  | Access Int
  | Equal
  | Jump Int
  | Return
  | Sub
  | Add
  deriving (Show)

data Env = Env
  { envSymbolLookup :: Map Text Int
  }

fibScheme :: Text
fibScheme = "(lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))"

fibSexpr :: Sexpr
fibSexpr = case parse sexpr "" fibScheme of
  Right result -> result
  Left err -> error (show err)

fibCompile :: [Inst]
fibCompile = runReader (compileSexpr fibSexpr) newEnv
  where
    newEnv :: Env
    newEnv = Env { envSymbolLookup = Map.empty }

compileSexpr :: Sexpr -> Reader Env [Inst]
compileSexpr ast = case ast of
    Atom (Symbol s) -> lookupSymbol s >>= \i -> return [ Access i ]
    Atom (Integer i) -> return [ Const i ]
    List [(Atom (Symbol "lambda")), (List params), body] -> compileLambda params body
    List [(Atom (Symbol "if")), test, consq, alt] -> compileIf test consq alt
    List ((Atom (Symbol fname)) : args) -> compileFuncCall fname args

compileLambda :: [Sexpr] -> Sexpr -> Reader Env [Inst]
compileLambda params body =
  local addParamsToEnv $ do
    bodyBlock <- compileSexpr body
    return $ concat
      [ bodyBlock
      , [ Return ]
      ]
  where
    addParamsToEnv :: Env -> Env
    addParamsToEnv env@(Env m) = env { envSymbolLookup = combineParams m }
    combineParams :: Map Text Int -> Map Text Int
    combineParams m = Map.union m (Map.fromList $ zip (paramsToSymbols params) [0..])
    paramsToSymbols :: [Sexpr] -> [Text]
    paramsToSymbols = map $ \case
      Atom (Symbol s) -> s
      _ -> error "param not a symbol"

compileIf test consq alt = do
  testBlock <- compileSexpr test
  consqBlock <- compileSexpr consq
  altBlock <- compileSexpr alt
  return $ concat
    [ testBlock
    , [ Jump $ length testBlock + length altBlock ]
    , consqBlock
    , altBlock
    ]

compileFuncCall fname args = do
  argBlock <- concat <$> mapM compileSexpr args
  return $ concat
    [ [ Closure $ ClosureName fname ]
    , argBlock
    , [ Apply ]
    ]

lookupSymbol :: Text -> Reader Env Int
lookupSymbol s = asks (Map.lookup s . envSymbolLookup) >>= \case
  Nothing -> error "symbol not found in env"
  Just i -> return i
