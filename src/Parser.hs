\{-# LANGUAGE NamedFieldPuns, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor, FlexibleContexts, LambdaCase #-}
module Parser where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Char
import Text.Printf

import System.IO.Unsafe

import qualified Control.Monad.Fail as F
import qualified Data.Map           as M
import qualified Data.Set           as S

import Util

data Program = Program { globals      :: M.Map Symbol GlobalDecl
                       , strings      :: String
                       } deriving (Show, Eq, Ord)

data ParseState = ParseState { lhs     :: String
                             , rhs     :: String
                             , loc     :: Location
                             , program :: Program
                             } deriving (Show, Eq)

newtype Parser a = Parser
  { runParser :: ParseState -> Either String (a, ParseState) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> case p s of
    Left err -> Left err
    Right (a, s') -> Right (f a, s')

instance Applicative Parser where
  pure x = Parser $ \s -> Right (x, s)
  (Parser pf) <*> (Parser pa) = Parser $ \s -> case pf s of
    Left err -> Left err
    Right (f, s') -> case pa s' of
      Left err -> Left err
      Right (a, s'') -> Right (f a, s'')

instance Alternative Parser where
  empty = F.fail "empty parse"
  (Parser p1) <|> (Parser p2) = Parser $ \s -> case p1 s of
    Right ok -> Right ok
    Left _ -> p2 s

instance Monad Parser where
  (Parser p) >>= f = Parser $ \s -> case p s of
    Left err -> Left err
    Right (a, s') -> runParser (f a) s'

instance MonadPlus Parser

instance MonadState ParseState Parser where
  state action = Parser (Right . action)

instance F.MonadFail Parser where
  fail msg = Parser $ \s -> Left (failFmt s msg)

failFmt :: ParseState -> String -> String
failFmt s msg = printf "%s [PARSER] %s" (loc s) msg
  
kill :: String -> Parser a
kill msg = Parser $ \s -> error $ failFmt s msg

todo :: String -> Parser a
todo msg = F.fail $ printf "`%s` is not yet implemented" msg

instance Semigroup a => Semigroup (Parser a) where
  p1 <> p2 = (<>) <$> p1 <*> p2
  

updateProgram :: (Program -> Program) -> Parser ()
updateProgram t = do
  ParseState lhs rhs loc prog <- get
  put $ ParseState lhs rhs loc (t prog)

initialParseState name input = ParseState
  "" input (Location name 1 1)
  (Program (M.fromList
            [("String",
              GlobalDecl Constant "String"
              (TypeLit $ StructType [("data",PrimType Addr),("count",PrimType Int)]))])
    [])

parseString :: Parser a -> String -> String -> Result a
parseString parser name input = fmap fst $
  runParser parser (initialParseState name input)

parseFile :: Parser a -> String -> IO (Result a)
parseFile parser path = readFile path >>= return . parseString parser path

debugParser :: Parser a -> String -> a
debugParser p str = case parseString p "debug" str of
  Right a  -> a
  Left msg -> error msg

debugParserFile :: Parser a -> FilePath -> IO a
debugParserFile parser path = readFile path >>= \s -> return $ debugParser parser s
  
-- Constructors
match :: (Char -> Bool) -> Parser Char
match p = do
  ParseState lhs rhs (Location file line col) globals <- get
  case rhs of
    [] -> F.fail "[match] reached end while parsing"
    x:xs -> if p x then do
      put $ if x == '\n' then
        ParseState (lhs++[x]) xs (Location file (line+1) 1) globals else
        ParseState (lhs++[x]) xs (Location file line (col+1)) globals
      return x
            else F.fail "[match] predicate failed"

matchAny :: Parser Char
matchAny = match (const True)

matchChar :: Char -> Parser Char
matchChar = match . (==)

matchString :: String -> Parser String
matchString = sequenceA . (map matchChar)

matchMany :: (Char -> Bool) -> Parser String
matchMany = many . match

matchSome :: (Char -> Bool) -> Parser String
matchSome = some . match

matchFromTo :: String -> String -> Parser String
matchFromTo from to = (matchString from)
                   <> (concat <$> many (mapM (match . (/=)) to))
                   <> (matchString to)

matchMap :: (Ord k, Show k) => M.Map k String -> Parser k
matchMap map = msum (matchMapKey map <$> M.keys map)

matchMapKey :: (Ord k, Show k) => M.Map k String -> k -> Parser k
matchMapKey map key = case M.lookup key map of
  Just value -> matchString value >> return key
  Nothing    -> F.fail $ printf "[matchMapKey] key '%s' is not a key in the map" (show key)

matchEnumValue :: (Enum a, Show a) => a -> Parser a
matchEnumValue enum = matchString (show enum) >> return enum

-- Parser Decorators
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

trim :: Parser a -> Parser a
trim p = ws *> p <* ws

-- NOTE this is a modified version of the `some` decorator which turns the entire file into a list of whatever you want to parse
complete :: Parser a -> Parser [a]
complete p = (:) <$> p <*> ((pEof *> return []) <|> (complete p))

-- Parsers
ws :: Parser String
ws = matchMany isSpace

pEof :: Parser ()
pEof = do
  ParseState _ rhs _ _ <- get
  if null rhs
    then return ()
    else F.fail "[eof] parser not at end of file"

pEnum :: (Enum a, Show a) => Parser a
pEnum = msum $ matchEnumValue <$> genEnum

-- The Tokenizer

data Intrinsic
  = Inc
  | Dec
  | Plus
  | Minus
  | Offset
  | Mul
  | Div
  | Mod
  | Eq
  | NEq
  | Len
  | Greater
  | Less
  | LeftShift
  | RightShift
  | And
  | Or
  | LoadAddr
  | LoadInt
  | LoadBool
  deriving(Show, Eq, Enum, Ord)

data IntrinsicInfo = IntrinsicInfo { intName    :: String
                                   , intType    :: ProcType
                                   } deriving (Show, Eq, Ord)

intrinsicInfos :: M.Map Intrinsic IntrinsicInfo
intrinsicInfos = M.fromList $
  [ (Plus,       IntrinsicInfo "+"   ([PrimType Int, PrimType Int], PrimType Int))
  , (Minus,      IntrinsicInfo "-"   ([PrimType Int, PrimType Int], PrimType Int))
  , (Offset,     IntrinsicInfo "offset" ([PrimType Addr, PrimType Int], PrimType Addr))
  , (Mul,        IntrinsicInfo "*"   ([PrimType Int, PrimType Int], PrimType Int))
  , (Div,        IntrinsicInfo "/"   ([PrimType Int, PrimType Int], PrimType Int))
  , (Mod,        IntrinsicInfo "%"   ([PrimType Int, PrimType Int], PrimType Int))
  , (Eq,         IntrinsicInfo "="   ([PrimType Int, PrimType Int], PrimType Bool))
  , (NEq,        IntrinsicInfo "!="  ([PrimType Int, PrimType Int], PrimType Bool))
  , (Inc,        IntrinsicInfo "inc" ([PrimType Int], PrimType Unit))
  , (Dec,        IntrinsicInfo "dec" ([PrimType Int], PrimType Unit))
  , (LeftShift,  IntrinsicInfo "shl" ([PrimType Int, PrimType Int], PrimType Int))
  , (RightShift, IntrinsicInfo "shr" ([PrimType Int, PrimType Int], PrimType Int))
  , (And,        IntrinsicInfo "&"   ([PrimType Int, PrimType Int], PrimType Int))
  , (Or,         IntrinsicInfo "|"   ([PrimType Int, PrimType Int], PrimType Int))
  , (Greater,    IntrinsicInfo ">"   ([PrimType Int, PrimType Int], PrimType Int))
  , (Less,       IntrinsicInfo "<"   ([PrimType Int, PrimType Int], PrimType Int))
  , (LoadAddr,   IntrinsicInfo "load-addr" ([PrimType Addr], PrimType Addr))
  , (LoadInt,    IntrinsicInfo "load-int" ([PrimType Int], PrimType Int))
  , (LoadBool,   IntrinsicInfo "load-bool" ([PrimType Bool], PrimType Bool))
  ]

data Keyword
  = KwLet
  | KwLetStar
  | KwProc
  | KwFunc
  | KwMacro
  | KwOpenParen
  | KwCloseParen
  | KwColon
  | KwEquals
  | KwOpenCurly
  | KwCloseCurly
  | KwLabel
  | KwJump
  | KwIf
  | KwElse
  | KwWhile
  | KwArrow
  | KwReturn
  | KwStruct
  | KwSyscall
  | KwSet
  deriving(Show, Eq, Enum, Ord)

keywordNames :: M.Map String Keyword
keywordNames = M.fromList $
  [ ("let*",    KwLetStar)
  , ("let",     KwLet)
  , ("(",       KwOpenParen)
  , (")",       KwCloseParen)
  , ("{",       KwOpenCurly)
  , ("}",       KwCloseCurly)
  , (":",       KwColon)
  , ("=",       KwEquals)
  , ("proc",    KwProc)
  , ("func",    KwFunc)
  , ("macro",   KwMacro)
  , ("label",   KwLabel)
  , ("jump",    KwJump)
  , ("if",      KwIf)
  , ("else",    KwElse)
  , ("while",   KwWhile)
  , ("->",      KwArrow)
  , ("return",  KwReturn)
  , ("struct",  KwStruct)
  , ("syscall", KwSyscall)
  , ("set",     KwSet)
  ]

type Symbol = String

mkToken :: Parser a -> Parser a
mkToken p = pComment *> p <* pComment
  where pComment = (ws *> many (ws *>
                                ((matchFromTo ";--" ";-") <|> (matchFromTo ";" "\n"))
                                <* ws)
                     >> ws)

pIntrinsic :: Parser Intrinsic
pIntrinsic = mkToken $ matchMap (intName <$> intrinsicInfos) >>= return

-- NOTE the reverse is a hack to let longer keywords which contain other keywords to parse properly as (as far as I can tell) M.keys produces the keys of the map in lexicographic order
pKeyword :: Keyword -> Parser Keyword
pKeyword kw = mkToken $ do
  matched <- (msum (matchString <$> (reverse $ M.keys keywordNames))) >>= return . ((M.!) keywordNames)
  if matched == kw then return kw else
    F.fail $ printf "Expected keyword `%s`, but got `%s`" (show kw) (show matched)


pSymbol :: Parser Symbol
pSymbol = mkToken $ matchSome firstChars <> matchMany (not . (flip S.member illegalChars))
  where firstChars c = ((not . isUpper) c || (not . isDigit) c) && (not . S.member c) illegalChars
        illegalChars = S.fromList [ ':', ' ', '\n', '=', ';', ')', '(']

pIntLiteral :: Parser Int
pIntLiteral = mkToken $ read <$> (matchString "-" <|> matchSome isDigit) <> (matchMany isDigit)

pBoolLiteral :: Parser Bool
pBoolLiteral = mkToken $ (matchString "true" >> return True) <|> (matchString "false" >> return False)

pStringLiteral :: Parser String
pStringLiteral = mkToken $ matchChar '"' *> stringLiteral <* matchChar '"'
  where
    stringLiteral = (many noEscape <>
                     (matchString "\\" <> (return <$> (matchAny))) <>
                     stringLiteral)
                    <|> (many noEscape)
    noEscape = match (\c -> (c /= '"') && (c /= '\\'))

pStructLiteral :: Parser StructLiteral
pStructLiteral = F.fail "struct literals are not yet implemented"

-- The lexer
pGlobalDecl :: Parser GlobalDecl
pGlobalDecl = do
  mutability <- (pKeyword KwLetStar >> return Variable) <|> (pKeyword KwLet >> return Constant)
  name       <- pSymbol
  maybeType  <- optional $ pKeyword KwColon *> pType
  typedExpr <- pKeyword KwEquals *> if name /= "start"
    then pTypedValue maybeType
    else case maybeType of
    Just (ProcType t@([PrimType Int,PrimType Addr],PrimType Unit)) ->
      ProcLambda <$> pProcLambda (Just t) True
    Just (ProcType ([], PrimType Unit)) -> pTypedValue maybeType
    Just t -> kill $ printf
      "`start` must either be `proc Addr Int -> Unit` or `proc -> Unit`, got %s" (show t)
    Nothing -> kill "type inference is not yet implemented"

  let decl = GlobalDecl mutability name typedExpr
  ParseState lhs rhs loc (Program globals strings) <- get
  put $ ParseState lhs rhs loc (Program (M.insert name decl globals) strings)
  return decl

pType :: Parser Type
pType = pPrimitiveType <|> pDefinedType <|> pStructType <|> pProcType

pPrimitiveType :: Parser Type
pPrimitiveType = mkToken $ PrimType <$> pEnum

pStructType :: Parser Type
pStructType = StructType <$> do
  pKeyword KwStruct >> pKeyword KwOpenCurly
  fields <- some $ (,) <$> (pSymbol <* pKeyword KwColon) <*> pType
  pKeyword KwCloseCurly
  return fields

pProcType :: Parser Type
pProcType = ProcType <$> ((,)
  <$> (pKeyword KwProc >> many pType)
  <*> ((pKeyword KwArrow >> pType) <|> return (PrimType Unit)))

pDefinedType :: Parser Type
pDefinedType = mkToken $ do
  name <- pSymbol
  globals <- globals . program <$> get
  case M.lookup name globals of
    Just (GlobalDecl _ _ (TypeLit tl)) -> return tl
    Just t -> F.fail $ printf "`%s` is not a defined type" (show t)
    Nothing -> F.fail $ printf "`%s` is not a type" name

escapedStringLength :: String -> Int
escapedStringLength str =
  (length str) - ((length . (filter (\c -> c == '\\'))) str)

pTypedValue :: Maybe Type -> Parser TypedValue
pTypedValue (Just (PrimType Int))  = Integer <$> pIntLiteral
pTypedValue (Just (PrimType Bool)) = Boolean <$> pBoolLiteral
pTypedValue (Just (PrimType Type)) = TypeLit <$> pType
pTypedValue (Just (StructType [("data",PrimType Addr),("count",PrimType Int)])) = do
  stringLit     <- pStringLiteral
  currentOffset <- escapedStringLength . strings . program <$> get
  updateProgram $ \Program{globals, strings} ->
    Program globals (strings ++ stringLit ++ "\\0 ") -- NOTE: gross hack to stop \0 becoming an octal literal if the next string is numbers
  return $ StructLit [ ("data",  Address (StringAddr currentOffset))
                     , ("count", Integer (escapedStringLength stringLit))
                     ]
pTypedValue (Just (ProcType ptExpected)) = ProcLambda <$> pProcLambda (Just ptExpected) False
pTypedValue (Just t) = F.fail $ printf "globals of type `%s` are not supported" (show t)
pTypedValue Nothing =
  (pTypedValue (Just (PrimType Int)))  <|>
  (pTypedValue (Just (PrimType Bool))) <|>
  (pTypedValue (Just (PrimType Type))) <|>
  (pTypedValue (Just (StructType [("data",PrimType Addr),("count",PrimType Int)])))

type ProcParser = StateT ProcInfo Parser
pProcLambda :: Maybe ProcType -> Bool -> Parser ProcLambda
pProcLambda mType entryPoint = runStateT
  ((pProcHeader <|> pGenHeaderInfoFromType) >> pProcExpr)
  (ProcInfo mType M.empty 0 (-8) 0)
  where
    allocateEntryArgs :: [(Symbol, Type)] -> ProcParser ()
    allocateEntryArgs [(arg1, PrimType Int), (arg2, PrimType Addr)] = do
      ProcInfo procType locals stackSize currentStackOffset labels <- get
      let newLocals = M.insert arg2 (LocalVarInfo (PrimType Addr) Constant 16) (M.insert arg1 (LocalVarInfo (PrimType Int) Constant 8) locals)
      put $ ProcInfo procType newLocals stackSize currentStackOffset labels
      -- error "THIS STILL DOESN'T WORK FIX IT"
    allocateEntryArgs a = lift $ kill $ printf "invalid type of entry `%s`" (show a)
    allocateArgs :: [(Symbol, Type)] -> ProcParser ()
    allocateArgs args = foldM_ allocateVar 1 args
      where allocateVar :: Int -> (Symbol, Type) -> ProcParser Int
            allocateVar n (name, typ) = do
              let returnSize = 8
              -- forM_ (zip [1..] args) $ \(n, (name, typ)) -> do
              -- NOTE from https://zhu45.org/images/stack-frame-structure.png but updated for 64bit
              let argStackOffset = returnSize + (8 * n)
              ProcInfo procType locals stackSize currentStackOffset labels <- get
              put $ ProcInfo procType (M.insert name (LocalVarInfo typ Constant argStackOffset) locals) stackSize currentStackOffset labels
              case typ of
                StructType fields -> foldM
                  (\n (suffix, fieldType) -> do
                      let argStackOffset = returnSize + (8 * n)
                      ProcInfo procType locals stackSize currentStackOffset labels <- get
                      let newLocals = M.insert (name++"."++suffix)
                                      (LocalVarInfo fieldType Constant argStackOffset) locals
                      put $ ProcInfo procType newLocals stackSize currentStackOffset labels
                      return $ n + 1
                  ) n fields
                _ -> return $ n + 1

    pProcHeader :: ProcParser ()
    pProcHeader = do
      lift $ pKeyword KwProc
      
      args <- lift $ many ((,) <$> pSymbol <*> (pKeyword KwColon *> pType))
      let argTypes = map snd args
      if entryPoint then allocateEntryArgs args else allocateArgs args
      
      retType <- lift $ (pKeyword KwArrow *> pType) <|> (return (PrimType Unit))

      let procTypeFromHeader = (argTypes, retType)
      
      ProcInfo mType locals stackSize stackOffset labels <- get
      case mType of
        Nothing -> put $ ProcInfo (Just procTypeFromHeader) locals stackSize stackOffset labels
        Just typ -> if procTypeFromHeader == typ then return () else lift $ kill $ printf
          "Expected procedure of type `%s`, but got `%s`" (show typ) (show procTypeFromHeader)
      
    pGenHeaderInfoFromType :: ProcParser ()
    pGenHeaderInfoFromType = (procType <$> get) >>= \case
        Nothing -> F.fail "cannot infer type of procedure"
        Just (argTypes, _) ->
          let args = zipWith (,) ((\i -> "arg"++(show i)) <$> [1..]) argTypes
          in if entryPoint then allocateEntryArgs args else allocateArgs args

    pProcBody :: ProcParser ProcExpr
    pProcBody = ProcBlock <$> ((lift $ pKeyword KwOpenCurly) *> some pProcExpr <* (lift $ pKeyword KwCloseCurly))
    pProcExpr :: ProcParser ProcExpr
    pProcExpr = pProcBlock <|> pProcGroup
      <|> pProcDeclareLocalWithLiteral <|> pProcDeclareLocalWithVariable
      <|> pProcDefinedCall <|> pProcSyscall
      <|> pProcEmitGlobal  <|> pProcEmitVar       <|> pProcEmitValue
      <|> pProcIntrinsicCall 
      <|> pProcIfElse      <|> pProcIf <|> pProcWhile
      <|> pProcLabel       <|> pProcJump
      <|> pProcReturn      <|> pProcSet
      where
        recordProcLocalVariable :: Type -> Mutability -> Symbol -> ProcParser ()
        recordProcLocalVariable structType@(StructType fields) mut structName = do
          ProcInfo procType locals sz offset labels <- get
          put $ ProcInfo
            procType
            (M.insert structName (LocalVarInfo structType mut offset) locals)
            sz offset labels
          forM_ fields $ \(name, typ) ->
            recordProcLocalVariable typ mut (structName++"."++name)
        recordProcLocalVariable typ mut name = do
          ProcInfo procTyp locals sz oldOffset labels <- get
          put $ ProcInfo
            procTyp
            (M.insert name (LocalVarInfo typ mut oldOffset) locals)
            (sz + 8)
            (oldOffset - 8)
            labels

        getNextLabelId :: ProcParser Int
        getNextLabelId = do
          ProcInfo a b c d label <- get
          put $ ProcInfo a b c d (label+1)
          return label

        pProcDeclareLocalWithLiteral = do
          (mut, name, typ, val) <- lift $ do
            mut  <- (pKeyword KwLet     >> return Constant)
                <|> (pKeyword KwLetStar >> return Variable)
            name <- pSymbol <|> kill "expected symbol after let statement"
            typ  <- (pKeyword KwColon *> pType <* pKeyword KwEquals) <|> kill "type inference is not yet implemented"
            val  <- pTypedValue (Just typ)
            return (mut, name, typ, val)
          recordProcLocalVariable typ mut name
          return $ ProcDeclareLocalWithLiteral name val
        pProcDeclareLocalWithVariable = do
          (typ, mut, name, ref) <- lift $ do
            mut  <- (pKeyword KwLet     >> return Constant)
                <|> (pKeyword KwLetStar >> return Variable)
            name <- pSymbol
                <|> kill "expected symbol after let statement"
            typ  <- (pKeyword KwColon *> pType <* pKeyword KwEquals)
                <|> kill "type inference is not yet implemented"
            ref  <- pSymbol
                <|> kill "can only assign to literals or variables"
            return (typ, mut, name, ref)
          recordProcLocalVariable typ mut name
          locals <- procLocals <$> get
          case M.lookup ref locals of
            Just info -> return $ ProcDeclareLocalWithVariable name info
            Nothing -> lift $ kill "can only declare variables with literals or locals as of now"
        pProcEmitGlobal = do
          name    <- lift $ pSymbol
          globals <- lift $ globals . program <$> get
          case M.lookup name globals of
            Just decl -> return $ ProcEmitGlobal decl
            Nothing   -> F.fail $ printf "`%s` is not a global variable" name
        pProcEmitVar = do
          name   <- lift $ pSymbol
          locals <- procLocals <$> get
          case M.lookup name locals of
            Just info -> return $ ProcEmitVar name info
            Nothing   -> F.fail $ printf "`%s` is not a local variable" name
        
        pProcGroup = ((lift $ pKeyword KwOpenParen) *> pProcExpr <* (lift $ pKeyword KwCloseParen))
        pProcBlock = ProcBlock <$>
          ((lift $ pKeyword KwOpenCurly) *> some pProcExpr <* (lift $ pKeyword KwCloseCurly))
        pProcEmitValue = ProcEmitValue <$> lift (pTypedValue Nothing)

        pProcIntrinsicCall = do
          int <- lift pIntrinsic
          let args = fst . intType $ intrinsicInfos M.! int
          -- TODO: typechecking
          exprs <- (forM args (const pProcExpr)) <|> (lift $ kill "could not parse arguments")
          return $ ProcIntrinsicCall int exprs
          
        pProcDefinedCall = pProcEmitGlobal >>= \case
          ProcEmitGlobal (GlobalDecl _ name (ProcLambda (_, ProcInfo (Just (argT, _)) _ _ _ _))) -> do
            args <- (replicateM (length argT) pProcExpr)
              <|> ((lift . kill) "could not parse function argument")
            let size = sum $ map sizeOf argT
            return $ ProcDefinedCall name args size
          ProcEmitGlobal (GlobalDecl _ name (ProcLambda (_, ProcInfo Nothing  _ _ _ _))) ->
            lift $ kill $ printf "type of procedure `%s` is not known" name
          other -> F.fail $ printf
            "`%s` is not a procedure" (show other)
        
        pProcSyscall = do
          lift $ pKeyword KwSyscall
          Integer nargs <- lift (pTypedValue (Just (PrimType Int)) <|> kill
            "expected integer literal after keyword `syscall`")
          args <- replicateM nargs (pProcExpr <|> (do locals <- procLocals <$> get
                                                      lift $ kill ("[syscall] could not parse expression:\n" ++ (show locals))))
          return $ ProcSyscall nargs args
        pProcIfElse = do
          condition <- (lift $ pKeyword KwIf) *> pProcExpr
          expr1     <- pProcExpr
          label1    <- getNextLabelId
          expr2     <- (lift $ pKeyword KwElse) *> pProcExpr
          label2    <- getNextLabelId
          return $ ProcIfElse condition expr1 label1 expr2 label2
        pProcIf = do
          condition <- (lift $ pKeyword KwIf) *> pProcExpr
          expr      <- pProcExpr
          label     <- getNextLabelId
          return $ ProcIf condition expr label
        pProcLabel  = ProcLabel <$> (lift (pKeyword KwLabel *> pSymbol))
        pProcJump   = do
          -- TODO: there should be some sort of check wether the symbol is a label or not
          lift $ pKeyword KwJump
          name <- lift pSymbol
          return $ ProcJump name
        pProcWhile = do
          lift $ pKeyword KwWhile
          condition   <- pProcExpr <|> (lift $ kill "could not parse while loop condition")
          body        <- pProcExpr <|> (lift $ kill "could not parse while look body")
          bodyId      <- getNextLabelId
          conditionId <- getNextLabelId
          ifLabel     <- getNextLabelId
          let conditionLabel = "L" ++ (show conditionId)
          let bodyLabel      = "L" ++ (show bodyId)
          -- NOTE: at some point when the language is self hosted it would be really cool if this is how macros are done generally like a parsing monad that just runs at compile time and returns an AST that is just like plopped in there
          return $ ProcBlock [ ProcJump conditionLabel , ProcLabel bodyLabel
                             , body
                             , ProcLabel conditionLabel
                             , ProcIf condition (ProcJump bodyLabel) ifLabel
                             ]
        pProcSet = do
          name <- lift $ pKeyword KwSet *> pSymbol <* pKeyword KwEquals
          locals <- procLocals <$> get
          case M.lookup name locals of
            Just (LocalVarInfo _ Variable offset) -> do
              expr <- pProcExpr
              return $ ProcSetLocal offset expr
            Just _ -> do
              lift $ kill "cannot set to immutable variable"
            Nothing -> do
              globals <- (globals . program) <$> (lift get)
              case M.lookup name globals of
                Just (GlobalDecl Variable _ _) -> lift $ kill "mutable globals are not yet implemented"
                Just _ -> lift $ kill $ printf "cannot set immutable variable `%s`" name
                Nothing -> lift $ kill $ printf "symbol `%s` is not a variable" name
        pProcReturn = lift $ pKeyword KwReturn >> kill "pProcReturn"

genProgram :: FilePath -> IO (Result Program)
genProgram path = parseFile pProgram path >>= return

pProgram :: Parser Program
pProgram = (complete pGlobalDecl) >> (program <$> get)

data GlobalDecl = GlobalDecl Mutability Symbol TypedValue
  deriving (Show, Eq, Ord)

data Mutability = Constant | Variable deriving (Show, Eq, Ord)

data TypedValue
  = Integer    Int
  | TypeLit    Type
  | Boolean    Bool
  | Address    Addr
  | StructLit  StructLiteral
  | ProcLambda ProcLambda
  deriving (Show, Eq, Ord)

data Addr = StringAddr Int
          | MemAddr Int
         deriving (Show, Eq, Ord)

type ProcLambda = (ProcExpr, ProcInfo)

data Type
  = PrimType   PrimType
  | StructType StructType
  | ProcType   ProcType
  deriving (Show, Eq, Ord)

sizeOf :: Type -> Int
sizeOf (StructType fields) = sum $ map (sizeOf . snd) fields
sizeOf _ = 8

data PrimType
  = Int
  | Addr
  | Bool
  | Unit
  | Type
  deriving (Enum, Show, Eq, Ord)

-- NOTE these names are very confusing
type StructType = [(String, Type)]

type StructLiteral = [(Symbol, TypedValue)]

type ProcType = ([Type], Type)

data ProcInfo = ProcInfo { procType           :: Maybe ProcType
                         , procLocals         :: M.Map Symbol LocalVarInfo
                         , procStackSize      :: Int
                         , currentStackOffset :: Int
                         , labelCount         :: Int
                         } deriving (Show, Eq, Ord)

data LocalVarInfo = LocalVarInfo { localVarType   :: Type
                                 , localVarMut    :: Mutability
                                 , localVarOffset :: Int
                                 } deriving (Show, Eq, Ord)

-- TODO: make TypedProcExpr that is (ProcExpr, Type, Location) for typechecking
-- bc now we are just losing type information besides sizes
data ProcExpr =
    ProcBlock [ProcExpr] -- TODO: add somekind of stack offset for shadowing???
  -- | ProcGroup
    
  | ProcDeclareLocalWithLiteral Symbol TypedValue
  | ProcDeclareLocalWithVariable Symbol LocalVarInfo

    -- TODO: remove ProcEmitGlobal and use EmitVar or EmitValue
  | ProcEmitGlobal GlobalDecl
  | ProcEmitVar    Symbol LocalVarInfo
  | ProcEmitValue  TypedValue -- TODO: change to EmitVal
  
  | ProcSetLocal Offset ProcExpr
  
  | ProcDefinedCall   Symbol    [ProcExpr] Int
  | ProcIntrinsicCall Intrinsic [ProcExpr]
  | ProcSyscall       Int       [ProcExpr]

  | ProcIfElse ProcExpr ProcExpr Int ProcExpr Int
  | ProcIf     ProcExpr ProcExpr Int
  | ProcLabel  String
  | ProcJump   String
  | ProcReturn (Maybe ProcExpr)
  deriving (Show, Eq, Ord)

type Offset = Int

data ProcImmediateValue
  = ProcImmediateString String
  | ProcImmediateBool   Bool
  | ProcImmediateType   Type
  | ProcImmediateInt    Int
  deriving (Show, Eq, Ord)

data ImmediateValue
  = ImmediateInt Int
  | ImmediateString String
  | ImmediateBool Bool
  | ImmediateType Type
  deriving (Show, Eq, Ord)

-- TODO: change this to be like type Caller = (Symbol, ProcType)
data Caller = Caller { callerName :: String
                     , callerArgs :: Type
                     , callerRet  :: Type
                     }
              deriving (Show, Eq, Ord)


