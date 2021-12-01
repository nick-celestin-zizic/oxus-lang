{-# LANGUAGE NamedFieldPuns, TypeSynonymInstances, FlexibleInstances, LambdaCase#-}
module Main where
import Control.Monad
import System.FilePath
import System.Process
import System.Exit
import Text.Printf
import qualified Data.Text as T

import Parser
import Lexer
import Compiler

main :: IO ()
main = do
  let srcPath   = "../examples/test2.ox"
  let outputDir = "../bin/"
  
  let exePath = outputDir ++ (takeBaseName srcPath)
  let objPath = exePath   ++ ".o"
  let asmPath = exePath   ++ ".asm"

  let compileCmd = printf "nasm -felf64 %s && ld %s -o %s"
                   asmPath objPath exePath

  
  program <- genProgram srcPath >>= \case
    Left msg   -> die msg
    Right prog -> return prog

  --print "---PROGRAM---"
  --forM_ program print
  
  src <- return (compileProgram program) >>= \case
    Left msg  -> die msg
    Right src -> return src

  {--
  print "----SOURCE BEGIN---"
  putStr src
  error "----SOURCE END---"
  --}
  
  writeFile asmPath src

  exitCode <- runCommand compileCmd >>= waitForProcess >>
              runCommand exePath    >>= waitForProcess >>= \case
          ExitSuccess   -> return 0
          ExitFailure i -> return i
  
  printf "exit code: %d\n" exitCode
