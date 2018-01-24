import Data.Char

data Operator = Plus | Minus | Times | Div
     deriving (Show, Eq)

data Token = TokOp Operator
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

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div
tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
  | elem c "+-*/" = TokOp (operator c) : tokenize cs
  | c == '(' = TokLParen : tokenize cs
  | c == ')' = TokRParen : tokenize cs
  | c == '{' = TokLBrace : tokenize cs
  | c == '}' = TokRBrace : tokenize cs
  | c == '=' = equalAfter TokEqEq TokEq cs
  | c == '!' = equalAfter TokBangEq TokBang cs
  | isDigit c = number c cs
  | c == '.' = TokDot : tokenize cs
  | c == '>' = equalAfter TokGThanEq TokGThan cs
  | c == '<' = equalAfter TokLThanEq TokLThan cs
  | isAlpha c = identifier c cs
  | isSpace c = tokenize cs
  | otherwise = error $ "Cannot tokenize :: String -> [Token]"


equalAfter :: Token -> Token -> String -> [Token]
equalAfter equalTok noEqualTok (c:cs) = if c == '='
                                        then equalTok : tokenize cs
                                        else noEqualTok : tokenize (c:cs)

identifier c cs = let (str, cs') = span isAlphaNum cs in
                    TokIdent (c:str) : tokenize cs'

number c cs =
  let (digs, cs') = span isNumberPart cs in
    TokNum (read (c:digs)) : tokenize cs'

isNumberPart :: Char -> Bool
isNumberPart c = isDigit c || c == '.'

main = do
  line <- getLine
  print $ tokenize line
