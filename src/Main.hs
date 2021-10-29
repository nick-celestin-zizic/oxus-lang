{-# LANGUAGE NamedFieldPuns, TypeSynonymInstances, FlexibleInstances#-}
module Main where
import System.FilePath
-- import Control.Monad
-- import Data.Functor.Compose
-- import Numeric.Natural
import System.Process
import System.Exit
import Text.Printf
-- import qualified Control.Monad.Fail as F
import qualified Data.Text           as T

-- import Scanner
import Parser
import Lexer
import Compiler
-- import Util

main :: IO ()
main = do
  let path = "../examples/test.ox"
  let baseName = takeBaseName path
  let outputPath = "../bin/"
  let asmPath = outputPath ++ baseName ++ ".asm"
  let exePath = outputPath ++ baseName
  tokens <- tokenizeFile path
  case tokens >>= generateProgram >>= compileProgram of
      Left msg  -> die msg
      Right asm -> do
        writeFile asmPath (T.unpack asm)
        x <- (runCommand $ printf "nasm -felf64 %s && ld %s.o -o %s && %s"
               asmPath (outputPath++baseName) exePath exePath) >>=
             waitForProcess
        printf "exit code: %d\n" $ case x of
          ExitSuccess   -> 0
          ExitFailure i -> i
