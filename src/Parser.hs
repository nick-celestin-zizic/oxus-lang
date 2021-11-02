{-# LANGUAGE NamedFieldPuns, TypeSynonymInstances, FlexibleInstances#-}
module Parser where

import Control.Applicative
import Control.Monad.State
import Data.Char
import Data.Functor.Compose
import Text.Printf

import qualified Control.Monad.Fail as F
import qualified Data.Map           as M
import qualified Data.Set           as S

import Scanner
import Util

type Parser = Scanner Char Location
type ParseResult = ScanResult Char Location
  
instance F.MonadFail Parser where
  fail msg = Scanner $ \s@ScanState{scanState} ->
    ScanResult $ Left (s, printf "%s [PARSER]%s" scanState msg)
    
parseString :: Parser a -> String -> String -> ParseResult a
parseString parser name input = runScanner parser
                              $ ScanState "" input (Location name 1 1)

parseFile :: Parser a -> String -> IO (ParseResult a)
parseFile parser path = readFile path >>= return . parseString parser path

debugParser :: Parser a -> String -> a
debugParser p str = case parseString p "debug" str of
  ScanResult (Right (_, a))   -> a
  ScanResult (Left (_, msg)) -> error msg
  
-- Constructors
match :: (Char -> Bool) -> Parser Char
match p = do
  ScanState lhs rhs (Location file line col) <- get
  case rhs of
    [] -> F.fail "[match] reached end while parsing"
    (x@'\n'):xs -> if p x then
                     put (ScanState (lhs++[x]) xs
                         (Location file (line+1) 1)) >>
                     return x
                   else
                     F.fail "[match] predicate failed"
    x:xs -> if p x then
              put (ScanState (lhs++[x]) xs
                  (Location file line (col+1))) >>
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

-- Parser Decorators
notNull :: Parser [a] -> Parser [a]
notNull p = do xs <- p
               if null xs
                  then F.fail "[notNull] parser returned null"
                  else return xs

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

eof :: Parser a -> Parser a
eof p = do x <- p
           ScanState _ rhs _ <- get
           if null rhs
             then return x
             else F.fail "[eof] parser not at end of file"

trim :: Parser a -> Parser a
trim p = ws *> p <* ws

-- NOTE this name is probably too confusing

-- Parsers
ws :: Parser String
ws = matchMany isSpace

pInt :: Parser Int
pInt = read <$> (matchString "-" <|> matchSome isDigit) <> (matchMany isDigit)

pUInt :: Parser UInt
pUInt = read <$> matchSome isDigit

pFloat2 :: Parser Float
pFloat2 = read <$> msum [ matchSome isDigit <> matchString "." <> (matchSome isDigit <|> return "0")
                       , return "0"        <> matchString "." <> matchSome isDigit
                       , matchSome isDigit
                       ]

pFloat :: Parser Float
pFloat = read <$> undefined

pString :: Parser String
pString = matchChar '"' *> stringLiteral <* matchChar '"'
  where
    stringLiteral = (many noEscape <>
                     (matchString "\\" <> (return <$> (matchAny))) <>
                     stringLiteral)
                    <|> (many noEscape)
    noEscape = match (\c -> (c /= '"') && (c /= '\\'))

-- The Tokenizer
data Token = Token { location :: Location
                   , kind :: TokenKind
                   }

instance Show Token where
  show Token{location, kind} = printf "%s %s" (show location) (show kind)



data TokenKind
  = Intrinsic (Intrinsic, IntrinsicInfo)
  | Keyword Keyword
  | Literal Literal
  | PrimitiveType PrimitiveType
  | Symbol Symbol
  deriving(Show, Eq, Ord)

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
                                   , intNumArgs :: Maybe Int -- Nothing means that we don't know (yet)
                                   } deriving (Show, Eq, Ord)

intrinsicInfos :: M.Map Intrinsic IntrinsicInfo
intrinsicInfos = M.fromList $
  [ (Plus,    IntrinsicInfo "+"  (Just 2))
  , (Minus,   IntrinsicInfo "-"  (Just 2))
  , (Eq,      IntrinsicInfo "==" (Just 2))
  , (Inc,     IntrinsicInfo "++" (Just 1))
  , (Dec,     IntrinsicInfo "--" (Just 1))
  , (Greater, IntrinsicInfo ">"  (Just 2))
  , (Less,    IntrinsicInfo "<"  (Just 2))
  , (Syscall, IntrinsicInfo "syscall" Nothing)
  , (Len,     IntrinsicInfo "len" (Just 1))
  ]

data Keyword
  = Var
  | Val
  | Proc
  | Func
  | Macro
  | LeftParen
  | RightParen
  | Colon
  | Equals
  | LeftCurly
  | RightCurly
  | Label
  | Jump
  | If
  | Arrow
  | Ret
  deriving(Show, Eq, Enum, Ord)

keywordNames :: M.Map Keyword String
keywordNames = M.fromList $
  [ (Val,        "val")
  , (Var,        "var")
  , (LeftParen,  "(")
  , (RightParen, ")")
  , (LeftCurly,  "{")
  , (RightCurly, "}")
  , (Colon,      ":")
  , (Equals,     "=")
  , (Proc,       "proc")
  , (Func,       "func")
  , (Macro,      "macro")
  , (Label,      "label")
  , (Jump,       "jump")
  , (If,         "if")
  , (Arrow,      "->")
  , (Ret,        "return")
  ]
  
data Literal
  = Integer Int
  | Str     String
  deriving(Show, Eq, Ord)

data PrimitiveType
  = Unit
  | I64
  | String
  deriving (Show, Eq, Ord, Enum)

type Symbol = String

tokenizeFile :: String -> IO (Result [Token])
tokenizeFile path = do
  src <- readFile path
  return $ do
    tokens <- getScanResult $ runScanner pTokens (ScanState "" src (Location path 1 0))
    -- TODO expand all includes
    return tokens

pTokens :: Parser [Token]
pTokens = eof $ some pToken

-- TODO comment parsing is still broken, can't have two comments right after eachother
pToken :: Parser Token
pToken = do
  ws
  many (ws *> matchFromTo ";" "\n" <* ws)
  ws
  s <- get
  token <- (pKeyword <|> pPrimitiveType <|> pIntrinsic <|> pLiteral <|> pSymbol)
  ws
  many (ws *> matchFromTo ";" "\n" <* ws)
  ws
  return $ Token (scanState s) token

pKeyword :: Parser TokenKind
pKeyword =  Keyword <$> matchMap keywordNames

pPrimitiveType :: Parser TokenKind
pPrimitiveType = PrimitiveType
                 <$> msum ((\pt -> matchString (show pt) >> return pt)
                 <$> (enumFrom (toEnum 0 :: PrimitiveType)))
          

pIntrinsic :: Parser TokenKind
pIntrinsic =  matchMap (intName <$> intrinsicInfos) >>= \int -> return $ Intrinsic (int, intrinsicInfos M.! int)

pLiteral :: Parser TokenKind
pLiteral = Literal <$> ((Integer <$> pInt) <|> (Str <$> pString))

pSymbol :: Parser TokenKind
pSymbol = Symbol <$> matchSome firstChars <> matchMany (not . (flip S.member illegalChars))
  where firstChars c = ((not . isUpper) c || (not . isDigit) c) && (not . S.member c) illegalChars
        illegalChars = S.fromList [ ':', ' ', '.', '\n', '=', ';']


