{-# LANGUAGE NamedFieldPuns, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor, FlexibleContexts #-}
module Compiler where
import Control.Monad.State.Strict
--import Control.Monad.Trans.State.Strict
import Control.Monad.State.Class
import Numeric.Natural
import Text.Printf
import qualified Data.Map           as M

import Scanner
import Parser
import Lexer
import Util


type SourceCode = String

data CompileState = CompileState { src              :: SourceCode
                                 , dataSection      :: SourceCode
                                 , stringsCount     :: Natural
                                 , scopesInfo       :: M.Map Scope ScopeInfo
                                 , currentScopeInfo :: ScopeInfo
                                 } deriving Show

data ScopeInfo = ScopeInfo { rbpOffset  :: Int
                           , symOffsets :: M.Map Symbol Int
                           , scopeId    :: Scope
                           , symTable   :: SymbolTable
                           } deriving Show

defaultScopeInfo :: M.Map Symbol Int -> Scope -> SymbolTable  -> ScopeInfo
defaultScopeInfo = ScopeInfo 8

setInfoToScope :: Scope -> Compiler
setInfoToScope s = do
  -- TODO make a stack of scopes instead of just deleting the current scope
  CompileState src bss scount infos _ <- scanState <$> get
  put $ ScanState [] [] $ CompileState src bss scount infos (infos M.! s)

incRbpOffset :: Compiler
incRbpOffset = do
  CompileState src bss scount infos (ScopeInfo offset so id st) <- scanState <$> get
  let newInfo = ScopeInfo (offset + 8) so id st
  put $ ScanState [] [] $
    CompileState src bss scount (M.insert id newInfo infos) newInfo

recordSymbolOffset :: Symbol -> Compiler
recordSymbolOffset sym = do
  CompileState src bss scount infos (ScopeInfo offset so id st) <- scanState <$> get
  put $ ScanState [] []  $
    CompileState src bss scount infos (ScopeInfo offset (M.insert sym offset so) id st)

-- This is definately scuffed but i dont have to write more instances so w/e
type Compiler = Scanner () CompileState ()

emitf :: (PrintfArg r) => String -> r -> Compiler
emitf fmt args = do
  CompileState src bss scount scopes info <- scanState <$> get
  put $ ScanState [] [] $ CompileState (src ++ (printf fmt args)) bss scount scopes info

emit :: String -> Compiler
emit add = do
  CompileState src bss scount scopes info <- scanState <$> get
  put $ ScanState [] [] $ CompileState (src ++ add) bss scount scopes info

emitBss :: String -> Compiler
emitBss add = do
  CompileState src bss scount scopes info <- scanState <$> get
  put $ ScanState [] [] $ CompileState src (bss++add) scount scopes info

emitBssf :: (PrintfArg r) => String -> r -> Compiler
emitBssf fmt args = do
  CompileState src bss scount scopes info <- scanState <$> get
  put $ ScanState [] [] $ CompileState src (bss ++ (printf fmt args)) scount scopes info

runCompiler :: Compiler -> Scopes -> Result SourceCode
runCompiler c vars = src <$>
  getScanState (runScanner c (ScanState [] [] $
                               CompileState "" "" 0
                               ((defaultScopeInfo M.empty Nothing) <$> vars)
                               (defaultScopeInfo M.empty Nothing (vars M.! Nothing))
                             ))
  
compileProgram :: Program -> Result SourceCode
compileProgram (Program (Procedure decl expr) scopes) = flip runCompiler scopes$ do
  emitBss " ;; Strings\n"
  emitBss "          section   .data\n"
  emit " ;; Startup\n"
  emit "          global    _start\n"
  emit "          section   .text\n"
  emit "_start:\n"
  emit "          push      rbp\n"
  emit "          mov       rbp, rsp\n"
  emit " ;; Program Start\n"
  setInfoToScope (Just decl)
  emitExpr expr
  emit " ;; Program End\n"
  emit "          mov       rax, 60\n"
  emit "          mov       rdi, 0\n"
  emit "          syscall\n"
  bss <- (dataSection . scanState) <$> get
  emit bss

emitExpr :: ProcExpr -> Compiler
emitExpr (Block exprs) = forM_ exprs emitExpr
emitExpr (ImmediateValue (Integer i)) =
  emitf "          mov       rax, %d\n" i
emitExpr (ImmediateValue (String s)) = do
  scount <- (stringsCount . scanState) <$> get
  emitBssf "str_%d:\n" scount
  emitBssf "          db `%s`\n" s
  
  emitf    "          lea       rax, [rel str_%d]\n" scount
  -- incStringsCount
emitExpr (Call int args) = case int of
  (Len, _) -> undefined
  (Plus, _) -> case args of
    ops@(op1:op2:[]) -> do
      emit " ;; Plus\n"
      ScopeInfo offset _ _ symTable <- currentScopeInfo <$> scanState <$> get
      emitExpr op1
      emit "          mov       rdx, rax\n"
      emitExpr op2
      emit "          add       rax, rdx\n"
    other -> error (show other)
  (Syscall, 2) -> case args of
    (op1:op2:[]) -> do
      emit " ;; Syscall 2\n"
      emitExpr op2
      emit "          mov       rdi, rax\n"
      emitExpr op1
      emit "          syscall\n"
    _ -> error "this should have been typechecked"
  (Syscall, 4) -> case args of
    (op1:op2:op3:op4:[]) -> do
      emit " ;; Syscall 4\n"
      emitExpr op4
      emit "          mov       rdx, rax\n"
      emitExpr op3
      emit "          mov       rsi, rax\n"
      emitExpr op2
      emit "          mov       rdi, rax\n"
      emitExpr op1
      emit "          syscall\n"
    _ -> error "this should have been typechecked"
  other -> error (show other)
emitExpr (Declaration sym expr) = do
  emitf " ;; Declaration of %s\n" (show sym)
  emitExpr expr
  -- TODO redeclaration of mutable variables should use the same offset
  offset <- rbpOffset <$> currentScopeInfo <$> scanState <$> get
  recordSymbolOffset sym
  emitf "          mov       QWORD -%d[rbp], rax\n" offset
  incRbpOffset
emitExpr (Identifier sym) = do
  offset <- ((M.! sym) . symOffsets . currentScopeInfo . scanState) <$> get
  emitf "          mov       QWORD rax, -%d[rbp]\n" offset
-- emitExpr expr = error $ show expr
