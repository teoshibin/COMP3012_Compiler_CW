{-

Compilers Course (COMP3012), 2020
  Venanzio Capretta (based on Henrik Nilsson 2006-2013)

Parser for Arithmetic expressions
Using Monadic Parsers

-}


module ExpParser where

import FunParser
import Control.Applicative

data BinOperator =
  Addition | Subtraction | Multiplication | Division
  deriving (Eq,Show)

data UnOperator = Negation
  deriving (Eq,Show)

data AST = LitInteger Integer
         | BinOp BinOperator AST AST
         | UnOp  UnOperator AST
  deriving (Eq,Show)

{- Grammar for expressions:
    exp ::= mexp | mexp + exp | mexp - exp
    mexp ::= term | term * mexp | term / mexp
    term ::= int | - term | ( exp )
-}

expr :: Parser AST
expr = exprM <|> exprPlus <|> exprMinus
  where exprM = exprm
        exprPlus = do t1 <- exprm
                      symbol "+"
                      t2 <- expr
                      return (BinOp Addition t1 t2)
        exprMinus = do t1 <- exprm
                       symbol "-"
                       t2 <- expr
                       return (BinOp Subtraction t1 t2)


exprm :: Parser AST
exprm = ter <|> terTimes <|> terDivide
  where ter = term
        terTimes = do t1 <- term
                      symbol "*"
                      t2 <- mexpr
                      return (BinOp Multiplication t1 t2)
         terDivide = do t1 <- term
                      symbol "/"
                      t2 <- mexpr
                      return (BinOp Division t1 t2)


term :: Parser AST
term = intLit <|> terNeg <|> terPar
  where intLit =





















