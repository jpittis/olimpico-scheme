{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Compiler
    ( fibCompile
    , ppInst
    , fibProg
    , fibDump
    ) where

import Data.Text (Text)
import Text.Megaparsec (parse)
import Control.Monad.Reader (Reader, runReader, asks, local)
import Data.Map (Map)
import qualified Data.Map as Map (empty, lookup, fromList, union)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as ByteString (concat, writeFile)

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
fibScheme = "(lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))"

fibSexpr :: Sexpr
fibSexpr = case parse sexpr "" fibScheme of
  Right result -> result
  Left err -> error (show err)

fibCompile :: [Inst]
fibCompile = runReader (compileSexpr fibSexpr) newEnv
  where
    newEnv :: Env
    newEnv = Env { envSymbolLookup = Map.empty }

fibProg :: [Inst]
fibProg =
  let prog    = concat [mainBlock, fibBlock]
      nameMap = Map.fromList [("main", 0), ("fib", 6)] in
  simplifyProg nameMap prog
  where
    fibBlock  = adjustOffset fibCompile 6
    mainBlock =
      [ Closure (ClosureName "fib")
      , Const 30
      , Apply
      , Stop
      ]

fibDump = dump fibProg "fib.osb"

simplifyProg :: Map Text Int -> [Inst] -> [Inst]
simplifyProg nameMap prog = map (replaceName nameMap) prog
  where
    replaceName m (Closure (ClosureName ident)) =
      case Map.lookup ident m of
        Just offset -> Closure (ClosureOffset offset)
        Nothing     -> error "offset not found"
    replaceName m inst = inst

ppInst :: [Inst] -> IO ()
ppInst asm = mapM print asm >> return ()

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
  testBlock  <- compileSexpr test
  consqBlock <- compileSexpr consq
  altBlock   <- compileSexpr alt
  return $ concat
    [ testBlock
    , [ Jump $ instLength testBlock + instLength consqBlock + 3 ]
    , adjustOffset consqBlock (instLength testBlock + 2)
    , [ Return ]
    , adjustOffset altBlock (instLength testBlock + 3 + instLength consqBlock)
    ]

compileFuncCall fname args = do
  argBlock <- concat <$> mapM compileSexpr args
  case fname of
    "=" -> return $ concat [ argBlock, [ Equal ]]
    "+" -> return $ concat [ argBlock, [ Add ]]
    "-" -> return $ concat [ argBlock, [ Sub ]]
    _   -> return $ concat
      [ [ Closure $ ClosureName fname ]
      , adjustOffset argBlock 2
      , [ Apply ]
      ]

lookupSymbol :: Text -> Reader Env Int
lookupSymbol s = asks (Map.lookup s . envSymbolLookup) >>= \case
  Nothing -> error "symbol not found in env"
  Just i -> return i

instLength :: [Inst] -> Int
instLength = sum . map instSize

instSize :: Inst -> Int
instSize inst =
  case inst of
    Closure _ -> 2
    Const _   -> 2
    Access _  -> 2
    Jump _    -> 2
    _         -> 1

adjustOffset :: [Inst] -> Int -> [Inst]
adjustOffset asm n = map (addOffset n) asm
  where
    addOffset :: Int -> Inst -> Inst
    addOffset n (Jump i) = Jump $ i + n
    addOffset _ inst = inst

dump :: [Inst] -> FilePath -> IO ()
dump prog path =
  outputBytecode path $ concatMap toBytecode prog
  where
    toBytecode :: Inst -> [Int]
    toBytecode Return                           = [0]
    toBytecode Equal                            = [1]
    toBytecode Add                              = [2]
    toBytecode Sub                              = [3]
    toBytecode Apply                            = [7]
    toBytecode Stop                             = [8]
    toBytecode (Closure (ClosureOffset offset)) = [9,  offset]
    toBytecode (Jump offset)                    = [10, offset]
    toBytecode (Const int)                      = [11, int]
    toBytecode (Access int)                     = [12, int]
    outputBytecode :: FilePath -> [Int] -> IO ()
    outputBytecode path bytecode =
      ByteString.writeFile path (ByteString.concat $
        map (Binary.encode . (fromIntegral :: Int -> Binary.Word64)) bytecode)
