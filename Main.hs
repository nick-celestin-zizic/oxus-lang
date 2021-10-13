{-# LANGUAGE NamedFieldPuns, TypeSynonymInstances, FlexibleInstances#-}
module Main where
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

data Expression = Expression
lexTokens :: [Token] -> Result [Expression] 
lexTokens tokens = undefined

compileExpressions :: [Expression] -> Result String
compileExpressions exprs = undefined

generateAstFromTokens = undefined

compileAstToNasm = undefined

main :: IO ()
main = do
  let basePath = "basic"
  tokenRes <- tokenizeFile ("./"++basePath++".ox")
  case tokenRes >>= (compileAstToNasm . generateAstFromTokens) of
    Left  msg -> die msg
    Right asm -> do
      writeFile "basic.asm" asm
      runCommand $ printf "nasm -felf64 %s.nasm" basePath
      runCommand $ printf "ld -o %s %s.o" basePath basePath
      runCommand ("./"++basePath)
      return ()
      
