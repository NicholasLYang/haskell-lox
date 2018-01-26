module Parser
  where
import Tokens

data Object = String | Double deriving Show
data Expr = BinaryNode Expr Token Expr
              | Grouping Expr
              | Literal Object
              | Unary Token Expr
           deriving Show

lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (c:cs) = c

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts

parse :: [Token] -> Expr
parse toks = let (expr, toks') = expression toks
             in
               if null toks'
               then expr
               else error $ "Leftover tokens: " ++ show toks'
               
expression :: [Token] -> (Expr, [Token])
expression = equality

equality toks =
  let (leftExpr, toks') = comparison toks
  in accumulateExpr
     [TokDoubleEqual, TokBangEqual]
     comparison
     (lookAhead toks')
     leftExpr
     (accept toks')

comparison :: [Token] -> (Expr, [Token])
comparison toks =
  let (leftExpr, toks') = addition toks
  in accumulateExpr
     [TokGreater, TokGreaterEqual, TokLess, TokLessEqual]
     addition
     (lookAhead toks')
     leftExpr
     (accept toks')

addition = undefined
  
-- If the next token is in the operatorToks then
-- parse the next tokens with nextFunc, bundle it
-- up in an Expr then call itself with the leftExpr
-- being a new BinaryNode. Otherwise, just return
-- the leftExpr
accumulateExpr :: [Token] -> ([Token] -> (Expr, [Token]))
               -> Token -> Expr -> [Token]
               -> (Expr, [Token])
accumulateExpr operatorToks nextFunc nextTok leftExpr toks =
  if elem nextTok operatorToks
  then
    let (rightExpr, toks') = nextFunc toks
    in accumulateExpr
       operatorToks
       nextFunc
       (lookAhead toks')
       (BinaryNode leftExpr nextTok rightExpr)
       (accept toks')
  else (leftExpr, toks)
