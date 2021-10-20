{-# LANGUAGE NamedFieldPuns, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor #-}
module Lexer where
import Control.Monad.State
import Control.Applicative
import Text.Printf
import Scanner
import Parser
import qualified Control.Monad.Fail as F
import qualified Data.Map           as M

import Util

type Lexer = Scanner Token LexState

instance F.MonadFail Lexer where
  fail msg = Scanner $ \s@ScanState{lhs} ->
    ScanResult $ Left (s, printf "%s [LEXER]: %s"
                                 ((show . location . last) lhs) msg)

endLex :: String -> Lexer a
endLex msg = do
  modify $ \ScanState{lhs, scanState} -> ScanState lhs [] scanState
  F.fail msg

generateProgram :: [Token] -> Result Program
generateProgram tokens = (getScanResult . runScanner lexProgram)
  (ScanState [] tokens initialLexState)

initialLexState :: LexState
initialLexState = LexState (M.fromList []) Nothing

getNextToken :: Lexer Token
getNextToken = get >>= \ScanState{lhs, rhs, scanState} ->
  case rhs of
    n:ns -> (put (ScanState (lhs ++ [n]) ns scanState)) >> return n
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

exIntrinsic :: Lexer (Intrinsic, Int)
exIntrinsic = getNextToken >>= \Token{kind} -> case kind of
    Intrinsic (int, info) -> case intNumArgs info of
      Just nargs -> return (int, nargs)
      Nothing -> do
        nargs <- exLiteral
        case nargs of
          Integer nargs -> return (int, nargs)
          other -> F.fail "Expected first argument of variadic function to be a number"
    _ -> F.fail $ printf "Expected token of kind Intrinsic, but got %s"
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


type Scope = Maybe ProcDecl
type SymbolTable = M.Map Symbol ProcExpr

data LexState = LexState { scopes :: M.Map Scope SymbolTable
                         , currentScope :: Maybe ProcDecl
                         } deriving (Show, Eq)

type Scopes = M.Map Scope SymbolTable

data Program = Program { entryPoint    :: Procedure,
                         programScopes :: Scopes
                       } deriving Show

data Procedure = Procedure ProcDecl ProcExpr
  deriving (Show, Eq, Ord)

data ProcDecl = ProcDecl Symbol PrimitiveType PrimitiveType deriving (Show, Eq, Ord)

data ProcExpr
  = Block [ProcExpr]
  | ImmediateValue Literal
  | Call (Intrinsic, Int) [ProcExpr]
  | Declaration Symbol ProcExpr
  | Identifier Symbol
  deriving (Show, Eq, Ord)

lexProgram :: Lexer Program
lexProgram = do
  exprs <- lexProc
  LexState scopes _ <- scanState <$> get
  return $ Program exprs scopes


lexProc :: Lexer Procedure
lexProc = do
  declName <- exSymbol
  exKeyword Colon
  argTypes <- (exPrimitive <|> return Unit)
  retTypes <- ((exKeyword Arrow >> exPrimitive) <|> return Unit)
  exKeyword Colon
  let procDecl = ProcDecl declName argTypes retTypes
  setCurrentScope procDecl
  procBody <- lexProcExpr
  return $ Procedure procDecl procBody

setCurrentScope :: ProcDecl -> Lexer ()
setCurrentScope decl = do
  ScanState lhs rhs (LexState scopes _) <- get
  put $ ScanState lhs rhs (LexState (M.insert (Just decl) M.empty scopes) (Just decl))

lexProcExpr :: Lexer ProcExpr
lexProcExpr = lexBlock <|> lexImmediateValue <|> lexDecl <|> lexBlock <|> lexSymbol <|> lexCall

addExprToCurrentScope :: Symbol -> ProcExpr -> Lexer ()
addExprToCurrentScope name val = do
  ScanState lhs rhs (LexState scopes currentScope) <- get
  case M.lookup currentScope scopes of
    Nothing -> put $ ScanState lhs rhs
      (LexState (M.insert currentScope (M.insert name val M.empty) scopes) currentScope)
    Just _ -> let newSymbolTable = M.adjust (M.insert name val) currentScope scopes in
      put $ ScanState lhs rhs (LexState newSymbolTable currentScope)

lexDecl :: Lexer ProcExpr
lexDecl = do
  name <- exSymbol
  exKeyword Colon >> exKeyword Colon
  expr <- lexProcExpr
  addExprToCurrentScope name expr
  return $ Declaration name expr

lexImmediateValue :: Lexer ProcExpr
lexImmediateValue = (ImmediateValue <$> exLiteral)

lexSymbol :: Lexer ProcExpr
lexSymbol = do
  sym <- exSymbol
  LexState{scopes, currentScope} <- scanState <$> get
  case M.lookup sym (scopes M.! currentScope) of
    Just _ -> return $ Identifier sym
    Nothing -> F.fail $ printf "symbol '%s' is not defined" sym

lexCall :: Lexer ProcExpr
lexCall = do
  pair@(_, nargs) <- exIntrinsic
  args <- forM [1..nargs] $ \_ -> lexProcExpr
  return $ Call pair args
  -- Call <$> exIntrinsic <*> some lexProcExpr

lexBlock :: Lexer ProcExpr
lexBlock = Block <$>
  (exKeyword Proc *> exKeyword LeftCurly *>
   many lexProcExpr
   <* exKeyword RightCurly)
