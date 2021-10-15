{-# LANGUAGE NamedFieldPuns, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor #-}
module Lexer where
import Control.Monad.State
import Text.Printf
import Scanner
import Parser
import qualified Control.Monad.Fail as F

import Util

data Program = Program Int
type Lexer = Scanner Token ()

instance F.MonadFail Lexer where
  fail msg = Scanner $ \s@ScanState{lhs} ->
    ScanResult $ Left (s, printf "%s [LEXER]: %s"
                                 ((show . location . last) lhs) msg)

generateProgram :: [Token] -> Result Program
generateProgram tokens = (getScanResult . runScanner lexProgram)
  (ScanState [] tokens ())

getNextToken :: Lexer Token
getNextToken = get >>= \ScanState{lhs, rhs} ->
  case rhs of
    n:ns -> (put $ ScanState (lhs ++ [n]) ns ()) >> return n
    []   -> F.fail "reached end while lexing"

exKeyword :: Keyword -> Lexer Keyword
exKeyword match = getNextToken >>= \Token{kind} ->
  case kind of
    Keyword target -> if match == target
      then return target
      else F.fail $ printf "Expected Keyword '%s', but got '%s'" (show match) (show target)
    _ -> F.fail $ printf "Expected token of kind Keyword %s, but got %s" (show match) (show kind)

exSymbol :: Lexer Symbol
exSymbol = getNextToken >>= \Token{kind} -> case kind of
    Symbol name -> return name
    _ -> F.fail $ printf "Expected token of kind Symbol, but got %s"
                         (show kind)
exPrimitive :: Lexer PrimitiveType
exPrimitive = getNextToken >>= \Token{kind} -> case kind of
    PrimitiveType name -> return name
    _ -> F.fail $ printf "Expected token of kind Symbol, but got %s"
                         (show kind)

exLiteral :: Lexer Literal
exLiteral = getNextToken >>= \Token{kind} -> case kind of
    Literal a -> return a
    _ -> F.fail $ printf "Expected token of kind Literal, but got %s"
                         (show kind)

exConstant :: Lexer Int
exConstant = do
  lit <- exLiteral
  case lit of
    Integer i -> return i
    _ -> error "todo"
  

lexProgram :: Lexer Program
lexProgram = (\arg ret body -> Program body)
           <$> exSymbol
           <*> (exKeyword Colon *> exKeyword Proc *>
                exPrimitive
                <* exKeyword Arrow <* exPrimitive <* exKeyword Equals)
           <*> exConstant
