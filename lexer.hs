module Lexer(tokenize)
  where
import Data.Char

data Token = TokSlash
           | TokDoubleSlash
           | TokStar
           | TokPlus
           | TokMinus
           | TokSemicolon
           | TokIdent String
           | TokNum Double
           | TokSpace
           | TokEq     -- =
           | TokLParen     -- (
           | TokRParen     -- )
           | TokLBrace     -- {
           | TokRBrace     -- }
           | TokEqEq         -- ==
           | TokBang       -- !
           | TokBangEq        -- !=
           | TokLThan      -- <
           | TokGThan      -- >
           | TokLThanEq -- <=
           | TokGThanEq -- >=
           | TokDot        -- .
           | TokVar
           | TokEnd
    deriving (Show, Eq)

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
  | c == '=' = doubleToken '=' TokEq TokEqEq cs
  | c == '!' = doubleToken '=' TokBang TokBangEq cs
  | isDigit c = number c cs
  | c == '.' = TokDot : tokenize cs
  | c == '>' = doubleToken '=' TokGThan TokGThanEq cs
  | c == '<' = doubleToken '=' TokLThan TokLThanEq cs
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
                    TokIdent (c:str) : tokenize cs'

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
