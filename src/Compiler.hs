{-# LANGUAGE NamedFieldPuns, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor, FlexibleContexts, LambdaCase #-}
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

data CompileState = CompileState
  { src               :: SourceCode
  , dataSection       :: SourceCode
  , stringsCount      :: Natural
  , stackBytesForProc :: Int
  , globals           :: M.Map Symbol (Type, ProcExpr)
  , scopeInfo         :: ScopeInfo
  } deriving Show

data ScopeInfo = ScopeInfo
  { rbpOffset  :: Int
  , symOffsets :: M.Map Symbol Int
  , dummy      :: ()
  , scopeDepth :: Natural
  , condCount  :: Int
  } deriving Show

defaultScopeInfo :: ScopeInfo
defaultScopeInfo = ScopeInfo (-8) M.empty () 0 0

decRbpOffset :: Compiler ()
decRbpOffset = do
  CompileState src dataS scount sbytes globals (ScopeInfo offset so st sd cc) <- scanState <$> get
  let newInfo = ScopeInfo (offset - 8) so st sd cc
  put $ ScanState [] [] $
    CompileState src dataS scount sbytes globals newInfo

incRbpOffset :: Compiler ()
incRbpOffset = do
  CompileState src dataS scount sbytes globals (ScopeInfo offset so st sd cc) <- scanState <$> get
  let newInfo = ScopeInfo (offset + 8) so st sd cc
  put $ ScanState [] [] $
    CompileState src dataS scount sbytes globals newInfo

incStringsCount :: Compiler ()
incStringsCount = do
  CompileState src dataS sc sb g si <- scanState <$> get
  put $ ScanState [] [] $
    CompileState src dataS (sc+1) sb g si

incCondCount :: Compiler ()
incCondCount = do
  CompileState src dataS scount sbytes globals (ScopeInfo offset so st sd cc) <- scanState <$> get
  let newInfo = ScopeInfo offset so st sd (cc+1)
  put $ ScanState [] [] $
    CompileState src dataS scount sbytes globals newInfo

recordSymbolOffset :: Identifier -> Compiler ()
recordSymbolOffset (sym, _) = do
  CompileState src dataS scount sbytes global (ScopeInfo offset so st sd cc) <- scanState <$> get
  put $ ScanState [] [] $
    CompileState src dataS scount sbytes global (ScopeInfo offset (M.insert sym offset so) st sd cc)

-- This is definately scuffed but i dont have to write more instances so w/e
type Compiler = Scanner () CompileState

emitf :: (PrintfArg r) => String -> r -> Compiler ()
emitf fmt args = do
  CompileState src dataS scount sbytes global info <- scanState <$> get
  put $ ScanState [] [] $ CompileState (T.append src (T.pack (printf fmt args))) dataS scount sbytes global info

emit :: String -> Compiler ()
emit add = do
  CompileState src dataS scount sbytes global info <- scanState <$> get
  put $ ScanState [] [] $ CompileState (T.append src (T.pack add)) dataS scount sbytes global info

emitDataS :: String -> Compiler ()
emitDataS add = do
  CompileState src dataS scount sbytes global info <- scanState <$> get
  put $ ScanState [] [] $ CompileState src (T.append dataS (T.pack add)) scount sbytes global info

emitDataSf :: (PrintfArg r) => String -> r -> Compiler ()
emitDataSf fmt = \typ -> do
  CompileState src dataS scount sbytes global info <- scanState <$> get
  put $ ScanState [] [] $
    CompileState src (T.append dataS (T.pack (printf fmt typ))) scount sbytes global info

runCompiler :: Compiler () -> Result SourceCode
runCompiler c = src <$>
  getScanState (runScanner c (ScanState [] [] $
                               CompileState T.empty T.empty 0 0 M.empty defaultScopeInfo))
  
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
  emitExpr (Block expr)
  dataS <- (dataSection . scanState) <$> get
  -- NOTE this is awful and really slow but i just want to get the language to the point where I can self host
  emit (T.unpack dataS)
generateProcBody :: ProcExpr -> Compiler (SourceCode, Int)
generateProcBody expr = do
    oldSrc <- (src . scanState) <$> get
    let oldLen = T.length oldSrc
    
    emitExpr expr
    
    CompileState newSrc ds so offset g si <- scanState <$> get
    put $ ScanState [] [] $ CompileState oldSrc ds so offset g si
    
    return (T.drop oldLen newSrc, (-(offset + 8)))

emitExpr :: ProcExpr -> Compiler ()
emitExpr (Block exprs) = do
  CompileState src dataS sCount sb g oldScope@(ScopeInfo ro so st sd cc) <- scanState <$> get
  put $ ScanState [] [] $ CompileState src dataS sCount sb g (ScopeInfo ro so st (sd+1) cc)
  
  forM_ exprs emitExpr
  CompileState src dataS sCount _ _ (ScopeInfo offset _ _ _ _) <- scanState <$> get
  put $ ScanState [] [] $ CompileState src dataS sCount offset g oldScope
emitExpr (ImmediateValue (Integer i)) =
  emitf "          mov       rax, %d\n" i
emitExpr (ImmediateValue (Str s)) = do
  scount <- (stringsCount . scanState) <$> get
  emitDataSf "str_%04d:" scount
  emitDataSf " db        `%s`\n" s
  emitf      "          lea       rax, [rel str_%04d]\n" scount
  incStringsCount
emitExpr (Declaration ("start", (Compound (Procedure [] (Primitive Unit)))) expr) = do
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
emitExpr (Declaration (name, typ@(Compound (Procedure args ret))) body) = do
  CompileState src ds sc sbfp gs oldInfo@(ScopeInfo ro so d sd cc) <- scanState <$> get

  --_ <- if name == "print_n_and_exit" then error (show args) else return ()
  forM_ (reverse args) $ \(sym, _) -> do
    CompileState src ds sc sbfp gs (ScopeInfo ro so d sd cc) <- scanState <$> get
    put $ ScanState [] [] $ CompileState src ds sc sbfp gs (ScopeInfo ro (M.insert sym (ro+24) so) d sd cc)
    incRbpOffset

  CompileState src ds sc sbfp gs (ScopeInfo ro so d sd cc) <- scanState <$> get
  put $ ScanState [] [] $ CompileState src ds sc sbfp (M.insert name (typ, body) gs) (ScopeInfo (-8) so d sd cc)
  
  emitf "%s:\n" name
  emit "          push      rbp\n"
  emit "          mov       rbp, rsp\n"
  
  (procBody, stackBytes) <- generateProcBody body
    
  emitf "          sub       rsp, %d\n" stackBytes
  emit (T.unpack procBody)

  emit  " ;; Procedure end\n"
  emitf "          add       rsp, %d\n" stackBytes
  emit  "          pop       rbp\n"
  emit  "          ret\n"
  CompileState src ds sc sbfp gs _ <- scanState <$> get
  put $ ScanState [] [] $ CompileState src ds sc sbfp gs oldInfo
emitExpr Return = do
  stackBytes <- ((\offset -> (-(offset + 8))) . rbpOffset . scopeInfo . scanState) <$> get
  emit  " ;; Procedure return\n"
  emitf "          add       rsp, %d\n" stackBytes
  emit  "          pop       rbp\n"
  emit  "          ret\n"
emitExpr (Declaration ident@(sym, typ) expr) = do
  CompileState src dataS sCount sb globals (ScopeInfo offset so st sd cc) <- scanState <$> get
  emitf " ;; Declaration of %s\n" (show sym)
  if sd > 1
    then do
      emitExpr expr
      -- TODO redeclaration of mutable variables should use the same offset
      --offset <- rbpOffset <$> scopeInfo <$> scanState <$> get
      recordSymbolOffset ident
      emitf "          mov       QWORD %d[rbp], rax\n" offset
      decRbpOffset
    else case expr of
      ImmediateValue i ->
        put $ ScanState [] [] $
          CompileState src dataS sCount sb (M.insert sym (typ, expr) globals) (ScopeInfo offset so st sd cc)
      expr -> error (show ident)

    --put $ ScanState [] [] $
      --CompileState src dataS sCount sb (ScopeInfo offset so (M.insert sym expr st) sd cc)
emitExpr (Lbl name) = do
  emitf ".label_%s:\n" name
emitExpr (Jmp name) = do
  emitf "          jmp       .label_%s\n" name
emitExpr (Name sym) = do
  offset <- ((M.lookup sym) . symOffsets . scopeInfo . scanState) <$> get
  case offset of
    Nothing -> do
      look <- ((M.lookup sym) . globals . scanState) <$> get
      case look of
        Nothing -> error $ printf "Symbol `%s` is not defined" sym
        Just (typ, expr) -> case typ of
          Primitive t -> emitExpr expr
          _ -> error (show expr)
    Just offset -> emitf "          mov       rax, QWORD %d[rbp]\n" offset
emitExpr (Call (DefinedCall name nargs) args) = do
  emitf " ;; Call to procedure %s\n" name
  look <- ((M.lookup name) . globals . scanState) <$> get
  let info@((Compound (Procedure _ _)), _) = case look of
        Just info -> info
        Nothing -> error "This should be unreachable, right?"
  forM_ args $ \arg -> emitExpr arg >> emit "          push      rax\n"
  emitf "          call      %s\n" name
  emitf "          add       rsp, %d\n" (length args * 8)
emitExpr (IfExpr expr exprs) = do
  condCount <- (condCount . scopeInfo . scanState) <$> get
  emitExpr expr
  emit  "          cmp       al, 0\n"
  emitf "          je        .cond_%d\n" condCount
  emitExpr exprs
  emitf ".cond_%d: \n" condCount
  incCondCount
emitExpr (Call (IntrinsicCall Greater 2) (op1:op2:[])) = do
      emit " ;; Greater\n"
      emitExpr op1
      emit "          mov       rdx, rax\n"
      emitExpr op2
      emit "          cmp       rdx, rax\n"
      emit "          setg      al      \n"
emitExpr (Call (IntrinsicCall Greater _) _) =
  error "Greater: this should have been typechecked"
emitExpr (Call (IntrinsicCall Less 2) (op1:op2:[])) = do
      emit " ;; Less\n"
      emitExpr op1
      emit "          mov       rdx, rax\n"
      emitExpr op2
      emit "          cmp       rdx, rax\n"
      emit "          setl      al      \n"
emitExpr (Call (IntrinsicCall Less _) _) =
  error "Less: this should have been typechecked"
emitExpr (Call (IntrinsicCall Plus 2) (op1:op2:[])) = do
      emit " ;; Plus\n"
      emitExpr op1
      emit "          mov       rdx, rax\n"
      emitExpr op2
      emit "          add       rax, rdx\n"
emitExpr (Call (IntrinsicCall Plus _) _) =
  error "Plus: this should have been typechecked"
emitExpr (Call (IntrinsicCall Minus 2) (op1:op2:[])) = do
      emit " ;; Minus\n"
      emitExpr op2
      emit "          mov       rdx, rax\n"
      emitExpr op1
      emit "          sub       rax, rdx\n"
emitExpr (Call (IntrinsicCall Minus _) _) =
  error "Plus: this should have been typechecked"
emitExpr (Call (IntrinsicCall Inc 1) ((Name op1):[])) =
  ((M.lookup op1) . symOffsets . scopeInfo . scanState) <$> get >>= \case
    Just offset -> emitf "          inc       QWORD %d[rbp]\n" offset
    Nothing     -> error $ printf "symbol %s does not exist" op1
emitExpr (Call (IntrinsicCall Inc _) _) =
  error "Inc: this should have been typechecked"
emitExpr (Call (IntrinsicCall Syscall 2) (op1:op2:[])) = do
      emit " ;; Syscall 2\n"
      emitExpr op2
      emit "          mov       rdi, rax\n"
      emitExpr op1
      emit "          syscall\n"
emitExpr (Call (IntrinsicCall Syscall 4) (op1:op2:op3:op4:[])) = do
      emit " ;; Syscall 4\n"
      emitExpr op4
      emit "          mov       rdx, rax\n"
      emitExpr op3
      emit "          mov       rsi, rax\n"
      emitExpr op2
      emit "          mov       rdi, rax\n"
      emitExpr op1
      emit "          syscall\n"
emitExpr (Call (IntrinsicCall Syscall n) _) =
  error $ printf  "Syscall %d: this should have been typechecked" n
emitExpr expr = error $ show expr
