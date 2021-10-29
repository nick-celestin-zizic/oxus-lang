{-# LANGUAGE NamedFieldPuns, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor, FlexibleContexts #-}
module Compiler where
import Control.Monad.State.Strict
--import Control.Monad.Trans.State.Strict
import Control.Monad.State.Class
import Numeric.Natural
import Text.Printf
import qualified Data.Text as T
import qualified Data.Map  as M

import Scanner
import Parser
import Lexer
import Util


type SourceCode = T.Text

data CompileState = CompileState { src              :: SourceCode
                                 , dataSection      :: SourceCode
                                 , stringsCount     :: Natural
                                 , stackBytesForProc :: Int
                                 , scopeInfo        :: ScopeInfo
                                 } deriving Show

data ScopeInfo = ScopeInfo { rbpOffset  :: Int
                           , symOffsets :: M.Map Symbol Int
                           , symTable   :: Scope
                           , scopeDepth :: Natural
                           , condCount  :: Int
                           } deriving Show

defaultScopeInfo :: ScopeInfo
defaultScopeInfo = ScopeInfo 8 M.empty M.empty 0 0

incRbpOffset :: Compiler ()
incRbpOffset = do
  CompileState src dataS scount sbytes (ScopeInfo offset so st sd cc) <- scanState <$> get
  let newInfo = ScopeInfo (offset + 8) so st sd cc
  put $ ScanState [] [] $
    CompileState src dataS scount sbytes newInfo

incStringsCount :: Compiler ()
incStringsCount = do
  CompileState src dataS sc sb si <- scanState <$> get
  put $ ScanState [] [] $
    CompileState src dataS (sc+1) sb si

incCondCount :: Compiler ()
incCondCount = do
  CompileState src dataS scount sbytes (ScopeInfo offset so st sd cc) <- scanState <$> get
  let newInfo = ScopeInfo offset so st sd (cc+1)
  put $ ScanState [] [] $
    CompileState src dataS scount sbytes newInfo

recordSymbolOffset :: Symbol -> Compiler ()
recordSymbolOffset sym = do
  CompileState src dataS scount sbytes (ScopeInfo offset so st sd cc) <- scanState <$> get
  put $ ScanState [] []  $
    CompileState src dataS scount sbytes (ScopeInfo offset (M.insert sym offset so) st sd cc)

-- This is definately scuffed but i dont have to write more instances so w/e
type Compiler = Scanner () CompileState

emitf :: (PrintfArg r) => String -> r -> Compiler ()
emitf fmt args = do
  CompileState src dataS scount sbytes info <- scanState <$> get
  put $ ScanState [] [] $ CompileState (T.append src (T.pack (printf fmt args))) dataS scount sbytes info

emit :: String -> Compiler ()
emit add = do
  CompileState src dataS scount sbytes info <- scanState <$> get
  put $ ScanState [] [] $ CompileState (T.append src (T.pack add)) dataS scount sbytes info

emitDataS :: String -> Compiler ()
emitDataS add = do
  CompileState src dataS scount sbytes info <- scanState <$> get
  put $ ScanState [] [] $ CompileState src (T.append dataS (T.pack add)) scount sbytes info

emitDataSf :: (PrintfArg r) => String -> r -> Compiler ()
emitDataSf fmt = \typ -> do
  CompileState src dataS scount sbytes info <- scanState <$> get
  put $ ScanState [] [] $
    CompileState src (T.append dataS (T.pack (printf fmt typ))) scount sbytes info

runCompiler :: Compiler () -> Result SourceCode
runCompiler c = src <$>
  getScanState (runScanner c (ScanState [] [] $
                               CompileState T.empty T.empty 0 0 defaultScopeInfo))
  
compileProgram :: Program -> Result SourceCode
compileProgram (Program expr) = runCompiler $ do
  emitDataS " ;; Strings\n"
  emitDataS "          section   .data\n"
  emit " ;; Startup\n"
  emit "          global    _start\n"
  emit "          section   .text\n"
  --emit "          push      rbp\n"
  --emit "          mov       rbp, rsp\n"
  emit " ;; Program Start\n"
  --error (show expr)
  emitExpr expr
  dataS <- (dataSection . scanState) <$> get
  -- NOTE this is awful and really slow but i just want to get the language to the point where I can self host
  emit (T.unpack dataS)
generateProcBody :: ProcExpr -> Compiler (SourceCode, Int)
generateProcBody expr = do
    oldSrc <- (src . scanState) <$> get
    let oldLen = T.length oldSrc
    
    emitExpr expr
    
    CompileState newSrc ds so offset si <- scanState <$> get
    put $ ScanState [] [] $ CompileState oldSrc ds so offset si
    
    return (T.drop oldLen newSrc, offset)

emitExpr :: ProcExpr -> Compiler ()
emitExpr (Block exprs) = do
  CompileState src dataS sCount sb oldScope@(ScopeInfo ro so st sd cc) <- scanState <$> get
  put $ ScanState [] [] $ CompileState src dataS sCount sb (ScopeInfo ro so st (sd+1) cc)
  forM_ exprs emitExpr
  CompileState src dataS sCount _ (ScopeInfo offset _ _ _ _) <- scanState <$> get
  put $ ScanState [] [] $ CompileState src dataS sCount (offset-8) oldScope
emitExpr (ImmediateValue (Integer i)) =
  emitf "          mov       rax, %d\n" i
emitExpr (ImmediateValue (String s)) = do
  scount <- (stringsCount . scanState) <$> get
  emitDataSf "str_%d:\n" scount
  emitDataSf "          db        `%s`\n" s
  emitf      "          lea       rax, [rel str_%d]\n" scount
  incStringsCount
emitExpr (Declaration "start" expr) = do
  emit "_start:\n"
  emit "          push      rbp\n"
  emit "          mov       rbp, rsp\n"

  (procBody, offset) <- generateProcBody expr
    
  emitf "          sub       rsp, %d\n" offset
  emit (T.unpack procBody)
  
  emit " ;; Program End\n"
  emit "          mov       rax, 60\n"
  emit "          mov       rdi, 0\n"
  emit "          syscall\n"
emitExpr (Declaration sym expr@(Block _)) = do
  CompileState src dataS sCount sb (ScopeInfo r so st sd cc) <- scanState <$> get
  put $ ScanState [] [] $ CompileState src dataS sCount sb (ScopeInfo r so (M.insert sym expr st) sd cc)
  
  emitf "%s:\n" sym
  emit "          push      rbp\n"
  emit "          mov       rbp, rsp\n"

  (procBody, offset) <- generateProcBody expr
    
  emitf "          sub       rsp, %d\n" offset
  emit (T.unpack procBody)

  emitf "          add       rsp, %d\n" offset
  emit "          pop       rbp\n"
  emit "          ret\n"
emitExpr (Declaration sym expr) = do
  CompileState src dataS sCount sb (ScopeInfo offset so st sd cc) <- scanState <$> get
  emitf " ;; Declaration of %s\n" (show sym)
  if sd > 1
    then do
      emitExpr expr
      -- TODO redeclaration of mutable variables should use the same offset
      --offset <- rbpOffset <$> scopeInfo <$> scanState <$> get
      recordSymbolOffset sym
      emitf "          mov       QWORD -%d[rbp], rax\n" offset
      incRbpOffset
    else put $ ScanState [] [] $
      CompileState src dataS sCount sb (ScopeInfo offset so (M.insert sym expr st) sd cc)
emitExpr (Identifier sym) = do
  expr <- ((M.lookup sym) . symTable . scopeInfo . scanState) <$> get
  case expr of
    Nothing -> do
      offset <- ((M.lookup sym) . symOffsets . scopeInfo . scanState) <$> get
      case offset of
        Nothing -> error $ printf "Symbol `%s` is not defined" sym
        Just offset -> emitf "          mov       rax, QWORD -%d[rbp]\n" offset
    Just expr -> case expr of
      Block _ -> do
        emitf " ;; Call to procedure %s\n" sym
        emitf "          call      %s\n" sym
      _ -> emitExpr expr
emitExpr (Call int args) = case int of
  (Len, _) -> case args of
    (op:[]) -> do
      error "Len is not yet implemenented"
    _ -> error "this should have been typechecked"
  (Greater, _) -> case args of
    (op1:op2:[]) -> do
      emit " ;; Greater\n"
      emitExpr op1
      emit "          mov       rdx, rax\n"
      emitExpr op2
      emit "          cmp       rdx, rax\n"
      --emit "          xor       rax, rax\n"
      emit "          setg      al      \n"
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
emitExpr (IfExpr expr exprs) = do
  condCount <- (condCount . scopeInfo . scanState) <$> get
  emitExpr expr
  emit "          cmp       al, 0\n"
  emitf "          je        .cond_%d\n" condCount
  emitExpr exprs
  emitf ".cond_%d: \n" condCount
  incCondCount
  return ()
--emitExpr expr = error $ show expr
