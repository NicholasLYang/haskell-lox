module Tokens(Token(..))
  where

data Token = TokSlash
           | TokDoubleSlash
           | TokStar
           | TokPlus
           | TokMinus
           | TokSemicolon        -- ;
           | TokComma            -- ,
           | TokIdent String
           | TokNum Double
           | TokString String
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
