{-

Compilers Course (COMP3012), 2021
  Venanzio Capretta
  Nicolai Kraus

Parser by Venanzio Capretta (based on Henrik Nilsson 2006-2013).
Parser for Arithmetic expressions, generating an Abstract Syntax
Tree from an expression (a string).
Uses functional parsers.

-}

module ExpParser where

import FunParser
import Control.Applicative

data BinOperator = Addition | Subtraction | Multiplication | Division
                 | Conjunction | Disjunction
                 | LssOp | LeqOp | GtrOp | GeqOp | EqOp | NeqOp
  deriving (Eq,Show,Enum)

data UnOperator = Negation | NegBool
  deriving (Eq,Show)

data AST = LitInteger Int
         | BinOp BinOperator AST AST
         | UnOp  UnOperator AST
         | Conditional AST AST AST
  deriving (Eq,Show)

-- Parse the top string: error if parsers fail or input not consumed

expParse :: String -> AST
expParse src = case parse expr src of
  [(t,"")] -> t
  _ -> error "Parsing error"

{-
Grammar for expressions (either arith or bool):
We allow arith expressions to occur as bool expressions in parentheses
Conditionals must be in parentheses, except at the top level

  exp ::= bexp | bexp ? bexp : bexp

  bexp ::= cexp | cexp || bexp
  cexp ::= bterm | bterm && cexp
  bterm ::= aexp | aexp `op` aexp
             where `op` is one of <,<=,>,>=,==,!=

  aexp ::= mexp | mexp + aexp | mexp - aexp
  mexp ::= aterm | aterm * mexp | aterm / mexp
  aterm ::= intLit | - aterm | ! aterm | ( exp )

-}

expr :: Parser AST
expr = do b <- bexp
          (do symbol "?"
              e0 <- bexp
              symbol ":"
              e1 <- bexp
              return (Conditional b e0 e1)
           <|>
           return b)

bexp :: Parser AST
bexp = do e0 <- cexp
          (do symbol "||"
              e1 <- bexp
              return (BinOp Disjunction e0 e1)
           <|>
           return e0)

cexp :: Parser AST
cexp = do e0 <- bterm
          (do symbol "&&"
              e1 <- cexp
              return (BinOp Conjunction e0 e1)
           <|>
           return e0)

-- Longer operators (eg "<=") must come before shorter ones ("<")
relop :: Parser BinOperator
relop = choice [ symbol "<=" >> return LeqOp
               , symbol "<"  >> return LssOp
               , symbol ">=" >> return GeqOp
               , symbol ">"  >> return GtrOp
               , symbol "==" >> return EqOp
               , symbol "!=" >> return NeqOp
               ]

bterm :: Parser AST
bterm = do e0 <- aexp
           (do op <- relop
               e1 <- aexp
               return (BinOp op e0 e1)
            <|>
            return e0) 


addminus :: Parser BinOperator
addminus = choice [ symbol "+" >> return Addition
                  , symbol "-" >> return Subtraction
                  ]

-- For left-associativity, we use an auxiliary function aexp'
--    that keeps a functional accumulator

aexp :: Parser AST
aexp = aexp' id

aexp' :: (AST -> AST) -> Parser AST
aexp' f = do e0 <- mexp
             (do op <- addminus
                 aexp' (BinOp op (f e0))
              <|>
              return (f e0))

multdiv :: Parser BinOperator
multdiv = choice [ symbol "*" >> return Multiplication
                 , symbol "/" >> return Division
                 ]

mexp :: Parser AST
mexp = mexp' id

mexp' :: (AST -> AST) -> Parser AST
mexp' f = do e0 <- aterm
             (do op <- multdiv
                 mexp' (BinOp op (f e0))
              <|>
              return (f e0))

aterm :: Parser AST
aterm = (natural >>= return . LitInteger)
        <|> (do symbol "-"
                e <- aterm
                return (UnOp Negation e))
        <|> (do symbol "!"
                b <- aterm
                return (UnOp NegBool b))
        <|> parens expr
