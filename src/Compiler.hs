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
                                 , scopeInfo        :: ScopeInfo
                                 } deriving Show

data ScopeInfo = ScopeInfo { rbpOffset  :: Int
                           , symOffsets :: M.Map Symbol Int
                           , symTable   :: Scope
                           } deriving Show

defaultScopeInfo :: ScopeInfo
defaultScopeInfo = ScopeInfo 8 M.empty M.empty

incRbpOffset :: Compiler
incRbpOffset = do
  CompileState src dataS scount (ScopeInfo offset so st) <- scanState <$> get
  let newInfo = ScopeInfo (offset + 8) so st
  put $ ScanState [] [] $
    CompileState src dataS scount newInfo

incStringsCount :: Compiler
incStringsCount = do
  CompileState src dataS sc si <- scanState <$> get
  put $ ScanState [] [] $
    CompileState src dataS (sc+1) si

recordSymbolOffset :: Symbol -> Compiler
recordSymbolOffset sym = do
  CompileState src dataS scount (ScopeInfo offset so st) <- scanState <$> get
  put $ ScanState [] []  $
    CompileState src dataS scount (ScopeInfo offset (M.insert sym offset so) st)

-- This is definately scuffed but i dont have to write more instances so w/e
type Compiler = Scanner () CompileState ()

emitf :: (PrintfArg r) => String -> r -> Compiler
emitf fmt args = do
  CompileState src dataS scount info <- scanState <$> get
  put $ ScanState [] [] $ CompileState (src ++ (printf fmt args)) dataS scount info

emit :: String -> Compiler
emit add = do
  CompileState src dataS scount info <- scanState <$> get
  put $ ScanState [] [] $ CompileState (src ++ add) dataS scount info

emitDataS :: String -> Compiler
emitDataS add = do
  CompileState src dataS scount info <- scanState <$> get
  put $ ScanState [] [] $ CompileState src (dataS++add) scount info

emitDataSf :: (PrintfArg r) => String -> r -> Compiler
emitDataSf fmt = \typ -> do
  CompileState src dataS scount info <- scanState <$> get
  put $ ScanState [] [] $
    CompileState src (dataS ++ (printf fmt typ)) scount info

runCompiler :: Compiler -> Result SourceCode
runCompiler c = src <$>
  getScanState (runScanner c (ScanState [] [] $
                               CompileState "" "" 0 defaultScopeInfo))
  
compileProgram :: Program -> Result SourceCode
compileProgram (Program expr) = runCompiler $ do
  emitDataS " ;; Strings\n"
  emitDataS "          section   .data\n"
  emit " ;; Startup\n"
  emit "          global    _start\n"
  emit "          section   .text\n"
  emit " ;; Program Start\n"
  --error (show expr)
  --setInfoToScope (Just decl)
  emitExpr expr
  dataS <- (dataSection . scanState) <$> get
  emit dataS

emitExpr :: ProcExpr -> Compiler
emitExpr (Block exprs _) = do
  CompileState _ _ _ oldScope <- scanState <$> get
  forM_ exprs emitExpr
  CompileState src dataS sCount _ <- scanState <$> get
  put $ ScanState [] [] $ CompileState src dataS sCount oldScope
emitExpr (ImmediateValue (Integer i)) =
  emitf "          mov       rax, %d\n" i
emitExpr (ImmediateValue (String s)) = do
  scount <- (stringsCount . scanState) <$> get
  emitDataSf "str_%d:\n" scount
  emitDataSf "          db `%s`\n" s
  emitf    "          lea       rax, [rel str_%d]\n" scount
  incStringsCount
emitExpr (Call int args) = case int of
  (Len, _) -> case args of
    (op:[]) -> do
      error "Len is not yet implemenented"
    _ -> error "this should have been typechecked"
  (Plus, _) -> case args of
    (op1:op2:[]) -> do
      emit " ;; Plus\n"
      emitExpr op1
      emit "          mov       rdx, rax\n"
      emitExpr op2
      emit "          add       rax, rdx\n"
    _ -> error "this should have been typechecked"
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
--emitExpr (Declaration "start" (Block exprs x)) =
  --emitExpr (Declaration "_start" (Block exprs x))
emitExpr e@(Declaration sym expr@(Block exprs x)) = do
  emitf "%s:\n" sym
  emit "          push      rbp\n"
  emit "          mov       rbp, rsp\n"
  emitExpr (Block exprs x)
  emit "          pop      rbp\n"
  case sym of
    "_start" -> do
      emit " ;; Program End\n"
      emit "          mov       rax, 60\n"
      emit "          mov       rdi, 0\n"
      emit "          syscall\n"
    _ -> emit "          ret\n"
  CompileState src dataS sCount (ScopeInfo r so st) <- scanState <$> get
  put $ ScanState [] [] $ CompileState src dataS sCount (ScopeInfo r so (M.insert sym expr st))
emitExpr (Declaration sym expr) = do
  emitf " ;; Declaration of %s\n" (show sym)
  CompileState src dataS sCount (ScopeInfo r so st) <- scanState <$> get
  put $ ScanState [] [] $ CompileState src dataS sCount (ScopeInfo r so (M.insert sym expr st))
  -- TODO use this dead code for mutable declarations and make it impossible to make global mutables (for now because its easier)
  --undefined
  -- emitExpr expr
  -- TODO redeclaration of mutable variables should use the same offset
  --offset <- rbpOffset <$> scopeInfo <$> scanState <$> get
  --recordSymbolOffset sym
  -- emitf "          mov       QWORD -%d[rbp], rax\n" offset
  --incRbpOffset
emitExpr (Identifier sym) = do
  expr <- ((M.lookup sym) . symTable . scopeInfo . scanState) <$> get
  case expr of
    Nothing -> error $ printf "Symbol `%s` is not defined" sym
    Just expr -> case expr of
      Block _ _ -> emitf "          call       %s\n" sym
      _ -> emitExpr expr
emitExpr expr = error $ show expr
