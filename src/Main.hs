module Main where
import System.FilePath
import System.Process
import System.Exit
import Text.Printf

import Parser
import Compiler

main :: IO ()
main = do
  let srcPath   = "./examples/bitwise.ox"
  let outputDir = "./bin/"
  
  let exePath = outputDir ++ (takeBaseName srcPath)
  let objPath = exePath   ++ ".o"
  let asmPath = exePath   ++ ".asm"

  let compileCmd = printf "nasm -felf64 %s && ld %s -o %s"
                   asmPath objPath exePath
  
  program <- (genProgram srcPath) >>= \case
    Left msg   -> die msg
    Right prog -> return prog
  
  {--
  forM_ program print
  undefined
  --}
  
  src <- return (compileProgram program) >>= \case
    Left msg  -> die msg
    Right src -> return src

  {--
  putStr src
  undefined
  --}
  
  writeFile asmPath src

  exitCode <- runCommand compileCmd >>= waitForProcess >>
              runCommand exePath    >>= waitForProcess >>= \case
          ExitSuccess   -> return 0
          ExitFailure i -> return i
  
  printf "exit code: %d\n" exitCode
