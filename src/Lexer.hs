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
  formatted <- F.fail msg
  error formatted

generateProgram :: [Token] -> Result Program
generateProgram tokens = (getScanResult . runScanner lexProgram)
  (ScanState [] tokens initialLexState)

initialLexState :: LexState
initialLexState = LexState M.empty

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
      Nothing -> exLiteral >>= \n -> case n of
        Integer nargs -> return (int, nargs)
        other -> F.fail $ printf "Expected first argument of variadic function to be a number but got %s" (show other)
    _ -> F.fail $ printf "Expected token of kind Intrinsic, but got %s" (show kind)

exType :: Lexer Type
exType = (Primitive <$> exPrimitiveType) <|> (Compound <$> exCompoundType)
  where
    exPrimitiveType = getNextToken >>= \Token{kind} -> case kind of
      Parser.PrimitiveType name -> return name
      _ -> F.fail $ printf "Expected token of kind Symbol, but got %s" (show kind)
    exCompoundType = do
      exKeyword Proc
      args <- many exIdentifier
      ret  <- Primitive <$> ((exKeyword Arrow *> exPrimitiveType) <|> (return Unit))
      return $ Procedure args ret

exIdentifier :: Lexer Identifier
exIdentifier = do
  name <- exSymbol
  exKeyword Colon
  typ <- exType
  return (name, typ)

exLiteral :: Lexer Literal
exLiteral = getNextToken >>= \Token{kind} -> case kind of
    Literal a -> return a
    _ -> F.fail $ printf "Expected token of kind Literal, but got %s"
                         (show kind)

type Scope = M.Map Identifier ProcExpr -- Maybe ProcDecl
type SymbolTable = M.Map Symbol ProcExpr

--data ProcInfo = 
data LexState = LexState { procs :: M.Map Symbol CallInfo
                         } deriving (Show, Eq)

--type Scopes = M.Map Scope SymbolTable

data Program = Program [ProcExpr] deriving Show

data CallInfo
  = IntrinsicCall Intrinsic Int
  | DefinedCall   Symbol    Int
  deriving (Show, Eq, Ord)

type Identifier = (Symbol, Type)

data Type
  = Primitive PrimitiveType
  | Compound  CompoundType
  deriving (Show, Eq, Ord)

data CompoundType
  = Procedure [Identifier] Type
  deriving (Show, Eq, Ord)

data ProcExpr
  = Block          [ProcExpr]
  | ImmediateValue Literal
  | Declaration    Identifier ProcExpr
  | Call           CallInfo   [ProcExpr]
  | Identifier     Identifier
  | Name           Symbol
  | IfExpr         ProcExpr   ProcExpr
  | Return
  deriving (Show, Eq, Ord)

lexProgram :: Lexer Program
lexProgram = Program <$> (some lexProcExpr)

lexProcExpr :: Lexer ProcExpr
lexProcExpr
   =  lexImmediateValue
  <|> lexReturn    
  <|> lexDecl
  <|> lexCall
  <|> lexIf
  <|> lexBlock
  <|> lexIdentifier
  <|> lexName
  <|> F.fail "could not lex"


lexReturn :: Lexer ProcExpr
lexReturn = exKeyword Ret >> return Return

lexDecl :: Lexer ProcExpr
lexDecl = do
  name <- exKeyword Val   *> exSymbol
  typ  <- exKeyword Colon *> exType
  case typ of
    Primitive _ -> do
      exKeyword Equals
      val <- lexImmediateValue
      return $ Declaration (name, typ) val
    Compound p@(Procedure args ret) -> do
      ScanState a b (LexState procs) <- get
      put $ ScanState a b
        (LexState (M.insert name (DefinedCall name (length args)) procs) )
      expr <- (exKeyword Equals *> lexProcExpr) <|> lexBlock
      return $ Declaration (name, typ) expr

lexImmediateValue :: Lexer ProcExpr
lexImmediateValue = (ImmediateValue <$> exLiteral)

lexIdentifier :: Lexer ProcExpr
lexIdentifier = do
  sym <- exSymbol
  typ <- exKeyword Colon *> exType
  return $ Identifier (sym, typ) {--
  case M.lookup sym (scopes M.! currentScope) of
    Just _ -> return $ Identifier sym
    Nothing -> F.fail $ printf "symbol '%s' is not defined" sym
  --}

lexName :: Lexer ProcExpr
lexName = Name <$> exSymbol

lexCall :: Lexer ProcExpr
lexCall = lexIntCall <|> lexProcCall
  where
    lexIntCall = do
      int@(name, nargs) <- exIntrinsic
      args <- replicateM nargs lexProcExpr
      return $ Call (IntrinsicCall name nargs) args
    lexProcCall = do
      name <- exSymbol
      procs <- (procs . scanState) <$> get
      case M.lookup name procs of
        Nothing -> F.fail $ printf "`%s` is not a defined procedure" name
        Just (DefinedCall _ nargs) -> do
          args <- replicateM nargs lexProcExpr
          return $ Call (DefinedCall name nargs) args

lexIf :: Lexer ProcExpr
lexIf = IfExpr <$>
  (exKeyword If *> lexProcExpr) <*> lexProcExpr

lexBlock :: Lexer ProcExpr
lexBlock = Block <$> (exKeyword LeftCurly *> (many lexProcExpr) <* exKeyword RightCurly)
