module Compiler where
import Text.Printf

import Parser
import Lexer
import Util

compileProgramToNasm :: Program -> Result String
compileProgramToNasm (Program entry) = Right $
  printf "          global    _start\n" ++
  printf "          section   .text\n"  ++
  printf "_start:\n" ++ compileProc entry

compileProc :: Procedure -> String
compileProc (Procedure _ expr) = compileProcExpr expr

compileProcExpr :: ProcExpr -> String
compileProcExpr (Block exprs) = exprs >>= compileProcExpr
compileProcExpr (Value (Integer n)) = show n
compileProcExpr (Value (String _)) = error "todo"
compileProcExpr (Call Syscall (op1:op2:op3:op4:_)) =
  printf "          mov       rax, %s\n" (compileProcExpr op1) ++
  printf "          mov       rdi, %s\n" (compileProcExpr op2) ++
  printf "          mov       rsi, %s\n" (compileProcExpr op3) ++
  printf "          mov       rdx, %s\n" (compileProcExpr op4) ++
  printf "          syscall\n"
compileProcExpr (Call Syscall (op1:op2:_)) =
  printf "          mov       rax, %s\n" (compileProcExpr op1) ++
  printf "          mov       rdi, %s\n" (compileProcExpr op2) ++
  printf "          syscall\n"
compileProcExpr (Call Syscall args) = error (show args)
compileProcExpr (Declaration _ _) = ""
compileProcExpr e = error (show e)
  
