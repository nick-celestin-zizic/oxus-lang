{-# LANGUAGE NamedFieldPuns, TypeSynonymInstances, FlexibleInstances, LambdaCase#-}
module Main where
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
  let path = "../examples/test.ox"
  let baseName = takeBaseName path
  let outputPath = "../bin/"
  let exePath = outputPath ++ baseName
  let asmPath = exePath    ++ ".asm"
  tokens <- tokenizeFile path
  case tokens >>= generateProgram >>= compileProgram of
      Left msg  -> die msg
      Right asm -> do
        let compileCmd = printf "nasm -felf64 %s && ld %s.o -o %s"
                         asmPath exePath exePath

        writeFile asmPath (T.unpack asm)
        
        exitCode <- runCommand compileCmd >>= waitForProcess >>
                    runCommand exePath    >>= waitForProcess >>= \case
          ExitSuccess   -> return 0
          ExitFailure i -> return i
          
        printf "exit code: %d\n" exitCode
