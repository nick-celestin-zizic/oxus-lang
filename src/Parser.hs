{-# LANGUAGE NamedFieldPuns, TypeSynonymInstances, FlexibleInstances, LambdaCase#-}
module Parser where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Char
import Data.Functor.Compose
import Text.Printf

import qualified Control.Monad.Fail as F
import qualified Data.Map           as M
import qualified Data.Set           as S

import Scanner
import Util

type Parser = Scanner Char ParseState
type ParseResult = ScanResult Char ParseState

data ParseState = ParseState { loc     :: Location
                             , globals :: M.Map Symbol GlobalDecl
                             } deriving (Show, Eq)

  
instance F.MonadFail Parser where
  fail msg = Scanner $ \s@ScanState{scanState} ->
    ScanResult $ Left (s, printf "%s [PARSER] %s" (loc scanState) msg)
    
parseString :: Parser a -> String -> String -> ParseResult a
parseString parser name input = runScanner parser
                              $ ScanState "" input (ParseState (Location name 1 1) M.empty)

parseFile :: Parser a -> String -> IO (ParseResult a)
parseFile parser path = readFile path >>= return . parseString parser path

debugParser :: Parser a -> String -> a
debugParser p str = case parseString p "debug" str of
  ScanResult (Right (_, a))   -> a
  ScanResult (Left (_, msg)) -> error msg

debugParserFile :: Parser a -> FilePath -> IO a
debugParserFile parser path = readFile path >>= \s -> return $ debugParser parser s
  
-- Constructors
match :: (Char -> Bool) -> Parser Char
match p = do
  ScanState lhs rhs (ParseState (Location file line col) globals) <- get
  case rhs of
    [] -> F.fail "[match] reached end while parsing"
    (x@'\n'):xs -> if p x then
                     put (ScanState (lhs++[x]) xs
                         (ParseState (Location file (line+1) 1) globals)) >>
                     return x
                   else
                     F.fail "[match] predicate failed"
    x:xs -> if p x then
              put (ScanState (lhs++[x]) xs
                  (ParseState (Location file line (col+1)) globals)) >>
              return x
            else
              F.fail "[match] predicate failed"

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
  ScanState _ rhs _ <- get
  if null rhs
    then return ()
    else F.fail "[eof] parser not at end of file"

pEnum :: (Enum a, Show a) => Parser a
pEnum = msum $ matchEnumValue <$> genEnum

pInt :: Parser Int
pInt = read <$> (matchString "-" <|> matchSome isDigit) <> (matchMany isDigit)

pBool :: Parser Bool
pBool = (matchString "true" >> return True) <|> (matchString "false" >> return False)

pUInt :: Parser UInt
pUInt = read <$> matchSome isDigit

pString :: Parser String
pString = matchChar '"' *> stringLiteral <* matchChar '"'
  where
    stringLiteral = (many noEscape <>
                     (matchString "\\" <> (return <$> (matchAny))) <>
                     stringLiteral)
                    <|> (many noEscape)
    noEscape = match (\c -> (c /= '"') && (c /= '\\'))

-- The Tokenizer

data Intrinsic
  = Inc
  | Dec
  | Plus
  | Minus
  | Eq
  | Syscall
  | Len
  | Greater
  | Less
  deriving(Show, Eq, Enum, Ord)

data IntrinsicInfo = IntrinsicInfo { intName    :: String
                                   , intType    :: ProcType
                                   } deriving (Show, Eq, Ord)

intrinsicInfos :: M.Map Intrinsic IntrinsicInfo
intrinsicInfos = M.fromList $
  [ (Plus,    IntrinsicInfo "+"  ([PrimType Int, PrimType Int], PrimType Int))
  , (Minus,   IntrinsicInfo "-"  ([PrimType Int, PrimType Int], PrimType Int))
  , (Eq,      IntrinsicInfo "==" ([PrimType Int, PrimType Int], PrimType Bool))
  , (Inc,     IntrinsicInfo "++" ([PrimType Int], PrimType Unit))
  , (Dec,     IntrinsicInfo "--" ([PrimType Int], PrimType Unit))
  , (Greater, IntrinsicInfo ">"  ([PrimType Int, PrimType Int], PrimType Int))
  , (Less,    IntrinsicInfo "<"  ([PrimType Int, PrimType Int], PrimType Int))
  ]

data Keyword
  = KwLet
  | KwLetStar
  | KwProc
  | KwFunc
  | KwMacro
  | KwLeftParen
  | KwRightParen
  | KwColon
  | KwEquals
  | KwOpenCurly
  | KwCloseCurly
  | KwLabel
  | KwJump
  | KwIf
  | KwIfStar
  | KwArrow
  | KwReturn
  | KwStruct
  deriving(Show, Eq, Enum, Ord)

keywordNames :: M.Map String Keyword
keywordNames = M.fromList $
  [ ("let*",   KwLetStar)
  , ("let",    KwLet)
  , ("(",      KwLeftParen)
  , (")",      KwRightParen)
  , ("{",      KwOpenCurly)
  , ("}",      KwCloseCurly)
  , (":",      KwColon)
  , ("=",      KwEquals)
  , ("proc",   KwProc)
  , ("func",   KwFunc)
  , ("macro",  KwMacro)
  , ("label",  KwLabel)
  , ("jump",   KwJump)
  , ("if*",    KwIfStar)
  , ("if",     KwIf)
  , ("->",     KwArrow)
  , ("return", KwReturn)
  , ("struct", KwStruct)
  ]

data Literal
  = Integer Int
  | Str     String
  deriving(Show, Eq, Ord)

type Symbol = String

mkToken :: Parser a -> Parser a
mkToken p = pComment *> p <* pComment
  where pComment    = ws *> many (ws *> matchFromTo ";" "\n" <* ws) >> ws

pIntrinsic :: Parser Intrinsic
pIntrinsic = mkToken $ matchMap (intName <$> intrinsicInfos) >>= return

pLiteral :: Parser Literal
pLiteral = mkToken ((Integer <$> pInt) <|> (Str <$> pString))

-- NOTE the reverse is a hack to let longer keywords which contain other keywords to parse properly as (as far as I can tell) M.keys produces the keys of the map in lexicographic order
pKeyword :: Keyword -> Parser Keyword
pKeyword kw = mkToken $ do
  matched <- (msum (matchString <$> (reverse $ M.keys keywordNames))) >>= return . ((M.!) keywordNames)
  if matched == kw then return kw else
    F.fail $ printf "Expected keyword `%s`, but got `%s`" (show kw) (show matched)


pSymbol :: Parser Symbol
pSymbol = mkToken $ matchSome firstChars <> matchMany (not . (flip S.member illegalChars))
  where firstChars c = ((not . isUpper) c || (not . isDigit) c) && (not . S.member c) illegalChars
        illegalChars = S.fromList [ ':', ' ', '\n', '=', ';']

pGlobalDecl :: Parser GlobalDecl
pGlobalDecl = do
  mutability <- (pKeyword KwLetStar >> return Variable) <|> (pKeyword KwLet >> return Constant)
  name <- pSymbol
  
  maybeType <- optional $ pKeyword KwColon *> pType

  pKeyword KwEquals
  typedExpr <- pTypedGlobalExpr maybeType

  let decl = GlobalDecl mutability name typedExpr
  recordDecl decl
  return decl

recordDecl :: GlobalDecl -> Parser ()
recordDecl decl@(GlobalDecl _ name _) = do
  ScanState lhs rhs (ParseState loc globals) <- get
  put $ ScanState lhs rhs (ParseState loc (M.insert name decl globals))

pType :: Parser Type
pType = pPrimitiveType <|> pStructType <|> pProcType <|> pDefinedType

pPrimitiveType :: Parser Type
pPrimitiveType = PrimType <$> pEnum

pStructType :: Parser Type
pStructType = StructType <$> do
  pKeyword KwStruct >> pKeyword KwOpenCurly
  fields <- some $ (,) <$> (pSymbol <* pKeyword KwColon) <*> pType
  pKeyword KwCloseCurly
  return fields

pProcType :: Parser Type
pProcType = ProcType <$> ((,)
  <$> (pKeyword KwProc >> (some pType <|> return [PrimType Unit]))
  <*> ((pKeyword KwArrow >> pType) <|> return (PrimType Unit)))

pDefinedType :: Parser Type
pDefinedType = do
  name <- pSymbol
  globals <- (globals . scanState) <$> get
  case M.lookup name globals of
    Just (GlobalDecl _ _ (TypeLiteral tl)) -> return tl
    Nothing -> F.fail $ printf "`%s` is not a type" name

--recordGlobal = undefined

pTypedGlobalExpr :: Maybe Type -> Parser TypedGlobalExpr
pTypedGlobalExpr (Just (PrimType Int))  = IntegerLiteral <$> pInt
pTypedGlobalExpr (Just (PrimType Bool)) = BooleanLiteral <$> pBool
pTypedGlobalExpr (Just (PrimType Type)) = TypeLiteral    <$> pType
pTypedGlobalExpr (Just (StructType [("data",PrimType Mem),("count",PrimType Int)])) = StringLiteral <$> pString
pTypedGlobalExpr (Just (ProcType ptExpected)) = ProcedureBody <$> pProcedureBody (Just ptExpected)
pTypedGlobalExpr (Just t) = F.fail $ printf
  "globals of type `%s` are not supported" (show t)
pTypedGlobalExpr Nothing = F.fail "type inference is not implemented yet"

todo :: (MonadFail m, PrintfArg t) => t -> m a
todo msg = F.fail $ printf "`%s` is not yet implemented" msg

type ProcParser = StateT ProcInfo Parser

pProcedureBody :: Maybe ProcType -> Parser ProcedureBody
pProcedureBody mType = runStateT
  ((pProcHeader <|> pGetHeaderInfoFromType) >>= pProcExpr) (ProcInfo mType M.empty 0 (-8))
  where
    pProcHeader :: ProcParser ProcType
    pProcHeader = do
      lift $ pKeyword KwProc
      
      args <- lift $ many ((,) <$> pSymbol <*> (pKeyword KwColon *> pType))
      let argTypes = map snd args
      
      forM_ (reverse args) $ \(name, typ) -> do
        ProcInfo mTyp locals stackSize currentStackOffset <- get
        put $ ProcInfo mTyp
          (M.insert name (LocalVarInfo typ Constant (currentStackOffset + 24)) locals)
          stackSize (currentStackOffset + 8)
      
      retType <- lift $ (pKeyword KwArrow *> pType) <|> (return (PrimType Unit))
      return (argTypes, retType)
      
    pGetHeaderInfoFromType :: ProcParser ProcType
    pGetHeaderInfoFromType = (procType <$> get) >>= \case
        Nothing -> F.fail "type inference is not that good yet, sry"
        Just procType@(argTypes, _) -> do
          let args = zipWith (,) ((\i -> "arg"++(show i)) <$> [1..]) argTypes

          forM_ (reverse args) $ \(name, typ) -> do
            ProcInfo mTyp locals stackSize currentStackOffset <- get
            put $ ProcInfo mTyp
              (M.insert name (LocalVarInfo typ Constant (currentStackOffset + 24)) locals)
              stackSize (currentStackOffset + 8)

          return procType
    
    pProcExpr :: ProcType -> ProcParser ProcExpr
    pProcExpr typeFromHeader = pProcLocalDecl <|> pProcGroup <|> pProcBlock <|> pProcImmediateValue <|> pProcPrefixCall <|> pProcInfixCall <|> pProcIf <|> pProcIfStar <|> pProcLabel <|> pProcJump <|> pProcReturn <|> pSyscall
      where
        pProcLocalDecl      = todo "pProcLocalDecl"
        pProcGroup          = todo "pProcGroup"
        pProcBlock          = todo "pProcBlock"
        pProcImmediateValue = todo "pProcImmediateValue"
        pProcPrefixCall     = todo "pProcPrefixCall"
        pProcInfixCall      = todo "pProcInfixCall"
        pProcIf             = todo "pProcIf"
        pProcIfStar         = todo "pProcIfStar"
        pProcLabel          = todo "pProcLabel"
        pProcJump           = todo "pProcJump"
        pProcReturn         = todo "pProcReturn"
        pSyscall            = todo "pSyscall"

type Program = [GlobalDecl]

genProgram :: FilePath -> IO (Result Program)
genProgram path = parseFile pProgram path >>= (return . getScanResult)

pProgram :: Parser Program
pProgram = complete pGlobalDecl

data GlobalDecl = GlobalDecl Mutability Symbol TypedGlobalExpr
  deriving (Show, Eq, Ord)

data Mutability = Constant | Variable deriving (Show, Eq, Ord)

data TypedGlobalExpr
  = IntegerLiteral Int
  | StringLiteral  String
  | TypeLiteral    Type
  | BooleanLiteral Bool
  | ProcedureBody  ProcedureBody
  deriving (Show, Eq, Ord)

type ProcedureBody = (ProcExpr, ProcInfo)

data Type
  = PrimType   PrimType
  | StructType StructType
  | ProcType   ProcType
  deriving (Show, Eq, Ord)

data PrimType
  = Int
  | Mem
  | Bool
  | Unit
  | Type
  deriving (Enum, Show, Eq, Ord)

type StructType = [(String, Type)]

type ProcType = ([Type], Type)

data CompExpr = CompIntegerLiteral Int
  deriving (Show, Eq, Ord)

data ProcExpr
  = ProcLocalDecl Symbol Type ProcExpr
  | ProcGroup ProcExpr
  | ProcBlock [ProcExpr]
  | ProcImmediateValue ImmediateValue
  | ProcPrefixCall Caller [ProcExpr]
  | ProcInfixCall  Caller ProcExpr [ProcExpr]
  | ProcIf ProcExpr ProcExpr
  | ProcIfStar ProcExpr ProcExpr ProcExpr
  | ProcLabel String
  | ProcJump String
  | ProcReturn (Maybe ProcExpr)
  deriving (Show, Eq, Ord)

data ImmediateValue
  = ImmediateInt Int
  | ImmediateString String
  | ImmediateBool Bool
  | ImmediateType Type
  deriving (Show, Eq, Ord)

data ProcInfo = ProcInfo { procType :: Maybe ProcType
                         , procLocals    :: M.Map Symbol LocalVarInfo
                         , procStackSize :: Int
                         , currentStackOffset :: Int
                         } deriving (Show, Eq, Ord)

data LocalVarInfo = LocalVarInfo { localVarType   :: Type
                                 , localVarMut    :: Mutability
                                 , localVarOffset :: Int
                                 } deriving (Show, Eq, Ord)

data Caller = Caller { callerName :: String
                     , callerArgs :: Type
                     , callerRet  :: Type
                     }
              deriving (Show, Eq, Ord)


