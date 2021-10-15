{-# LANGUAGE NamedFieldPuns, TypeSynonymInstances, FlexibleInstances#-}
module Main where
import Control.Monad
import Data.Functor.Compose
import Numeric.Natural
import System.Process
import System.Exit
import Text.Printf
import qualified Control.Monad.Fail as F

import Scanner
import Parser
import Lexer
import Compiler
import Util

{--
amain :: IO ()
amain = do
  let basePath = "basic"
  tokenRes <- tokenizeFile ("./"++basePath++".ox")
  case tokenRes >>= (compileProgramToNasm . generateAstFromTokens) of
    Left  msg -> die msg
    Right asm -> do
      writeFile "basic.asm" asm
      runCommand $ printf "nasm -felf64 %s.nasm" basePath
      runCommand $ printf "ld -o %s %s.o" basePath basePath
      runCommand ("./"++basePath)
      return ()
--}

compileProgramToNasm :: Program -> Result String
compileProgramToNasm (Program num) = Right $
  printf "          global    _start\n"++
  printf "          section   .text\n"++
  printf "_start:   mov       rax, 60\n"++
  printf "          mov       rdi, %d\n" num ++
  printf "          syscall\n"

main :: IO ()
main = do
  let basePath = "basic"
  tokens <- tokenizeFile ("./"++basePath++".ox")
  case tokens >>= generateProgram >>= compileProgramToNasm  of
      Left msg -> die msg
      Right asm -> do
        let asmPath = basePath++".asm"
        writeFile asmPath asm
        x <- (runCommand $ printf "nasm -felf64 %s && ld %s.o && ./a.out" asmPath basePath) >>=
             waitForProcess
        case x of
          ExitSuccess      -> print  "exit code 0"
          ExitFailure code -> printf "exit code %d\n" code
