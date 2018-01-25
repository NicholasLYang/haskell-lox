module Lexer(tokenize)
  where
import Data.Char
import qualified Data.Map as Map

data Token = TokSlash
           | TokDoubleSlash
           | TokStar
           | TokPlus
           | TokMinus
           | TokSemicolon        -- ;
           | TokComma            -- ,
           | TokIdent String
           | TokNum Double
           | TokSpace
           | TokEqual            -- =
           | TokLParen           -- (
           | TokRParen           -- )
           | TokLBrace           -- {
           | TokRBrace           -- }
           | TokDoubleEqual      -- ==
           | TokBang             -- !
           | TokBangEqual        -- !=
           | TokLess             -- <
           | TokGreater          -- >
           | TokLessEqual        -- <=
           | TokGreaterEqual     -- >=
           | TokDot              -- .
           | TokAnd
           | TokClass
           | TokElse
           | TokFalse
           | TokFor
           | TokFun
           | TokIf
           | TokNil
           | TokOr
           | TokPrint
           | TokReturn
           | TokSuper
           | TokThis
           | TokTrue
           | TokVar
           | TokWhile
           | TokEnd
    deriving (Show, Eq)

reservedWords :: Map.Map String Token
reservedWords = Map.fromList [ ("and", TokAnd)
                             , ("class", TokClass)
                             , ("else", TokElse)
                             , ("false", TokFalse)
                             , ("for", TokFor)
                             , ("fun", TokFun)
                             , ("if", TokIf)
                             , ("nil", TokNil)
                             , ("or", TokOr)
                             , ("print", TokPrint)
                             , ("return", TokReturn)
                             , ("super", TokSuper)
                             , ("this", TokThis)
                             , ("true", TokTrue)
                             , ("var", TokVar)
                             , ("while", TokWhile)
                             ]

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
  | c == '*' = TokStar : tokenize cs
  | c == ';' = TokSemicolon : tokenize cs
  | c == '-' = TokMinus : tokenize cs
  | c == '(' = TokLParen : tokenize cs
  | c == ')' = TokRParen : tokenize cs
  | c == '{' = TokLBrace : tokenize cs
  | c == '}' = TokRBrace : tokenize cs
  | c == '=' = doubleToken '=' TokEqual TokDoubleEqual cs
  | c == '!' = doubleToken '=' TokBang TokBangEqual cs
  | isDigit c = number c cs
  | c == '.' = TokDot : tokenize cs
  | c == '>' = doubleToken '=' TokGreater TokGreaterEqual cs
  | c == '<' = doubleToken '=' TokLess TokLessEqual cs
  | c == '/' = slash cs
  | isAlpha c = identifier c cs
  | isSpace c = tokenize cs
  | otherwise = error $ "Cannot tokenize :: String -> [Token]"

-- Checks if token consumes next char then produces the right token
-- Calls tokenize either with the remainder or with the head
doubleToken :: Char -> Token -> Token -> String -> [Token]
doubleToken c' singleToken doubleToken (c:cs) = if c == c'
                                        then doubleToken : tokenize cs
                                        else singleToken : tokenize (c:cs)

identifier c cs = let (str, cs') = span isAlphaNum cs in
                    case Map.lookup (c:str) reservedWords of
                      (Just tok) -> tok : tokenize cs'
                      Nothing  -> TokIdent (c:str) : tokenize cs'

number c cs =
  let (digs, cs') = span isNumberPart cs in
    TokNum (read (c:digs)) : tokenize cs'

isNumberPart :: Char -> Bool
isNumberPart c = isDigit c || c == '.'

slash :: String -> [Token]
slash [] = []
slash (c:cs) = if c == '/'
               then tokenize $ lineComment cs
               else TokSlash : tokenize cs

lineComment :: String -> String
lineComment [] = []
lineComment (c:cs)
  | c == '\n' = cs
  | otherwise = lineComment cs  
