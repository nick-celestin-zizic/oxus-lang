{-# LANGUAGE NamedFieldPuns, TypeSynonymInstances, FlexibleInstances#-}
module Main where
import Data.Functor.Compose
import Numeric.Natural
import System.Process
import System.Exit
import Text.Printf
import qualified Control.Monad.Fail as F

import Parser
import Lexer
import Compiler
import Util

data Token = Token
tokenizeSrc :: String -> Result [Token]
tokenizeSrc src = undefined

data Expression = Expression
lexTokens :: [Token] -> Result [Expression] 
lexTokens tokens = undefined

compileExpressions :: [Expression] -> Result String
compileExpressions exprs = undefined

compileFileToNasm :: String -> Result String
compileFileToNasm src = tokenizeSrc src >>= lexTokens >>= compileExpressions

main :: IO ()
main = do
  let basePath = "basic"
  src <- readFile ("./"++basePath++".ox")
  case compileFileToNasm src of
    Left  msg -> die msg
    Right asm -> do
      writeFile "basic.asm" asm
      runCommand $ printf "nasm -felf64 %s.nasm" basePath
      runCommand $ printf "ld -o %s %s.o" basePath basePath
      runCommand ("./"++basePath)
      return ()
      
