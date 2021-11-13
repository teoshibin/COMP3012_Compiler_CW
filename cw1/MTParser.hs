module MTParser where

import FunParser
import Control.Applicative

data AST = Program [Declaration] Command
         deriving (Eq,Show)

data Declaration = Var String Expr
                 deriving (Eq,Show)

data Command = CmdAssign String Expr
             | CmdIf Expr Command Command
             | CmdWhile Expr Command
             | CmdGetInt String
             | CmdPrintInt Expr
             | CmdBegin [Command]
             deriving (Eq,Show)

data Expr = LitInteger Int
          | BinOp BinOperator Expr
          | UnOp  UnOperator Expr
          | Ternary Expr Expr Expr
          deriving (Eq,Show)

data UnOperator = Negation | NegBool
                deriving (Eq,Show)

data BinOperator = Addition | Subtraction | Multiplication | Division
                 | Conjunction | Disjunction
                 | LssOp | LeqOp | GtrOp | GeqOp | EqOp | NeqOp
                 deriving (Eq,Show,Enum)


-- parse mini triangle
parseMT :: String -> AST
parseMT src = case parse programParser src of
    [(t, "")] -> t
    _ -> error "Parsing Error"

{-
NOTE 

keywords are reserved and are not valid identifiers / varaible names. 
let , in , var , if , then , else , while , do , getint , printint , begin , end

    program     ::= 'let' declarations 'in' command
    declaration ::= 'var' identifier | 'var' identifier ':=' expr
    declarations::= declaration | declaration ';' declarations
    command     ::= identifier ':=' expr
                | 'if' expr 'then' command 'else' command
                | 'while' expr 'do' command
                | 'getint' '(' identifier ')'
                | 'printint' '(' expr ')'
                | 'begin' commands 'end'
    commands ::= command | command ';' commands

-}

-- parse entire program
programParser :: Parser AST
programParser = do symbol "let"
                   d <- declarationsParser
                   symbol "in"
                   c <- commandParser
                   return (Program d c)

-- parse one declaration
declarationParser :: Parser Declaration
declarationParser = undefined

-- parse multiple declarations
declarationsParser :: Parser [Declaration]
declarationsParser = undefined

-- parse all types of Command
commandParser :: Parser Command
commandParser = do assignParser
            <|> do ifParser
            <|> do whileParser
            <|> do getIntParser
            <|> do printIntParser

assignParser :: Parser Command
assignParser = do i <- keyLessIdentifier
                  symbol ":="
                  e <- expr
                  return (CmdAssign i e)

ifParser :: Parser Command
ifParser = do symbol "if"
              e <- expr
              symbol "then"
              c0 <- commandParser
              symbol "else"
              c1 <- commandParser
              return (CmdIf e c0 c1)

whileParser :: Parser Command
whileParser = do symbol "while"
                 e <- expr
                 symbol "do"
                 c <- commandParser
                 return (CmdWhile e c)

getIntParser :: Parser Command
getIntParser = do symbol "getint"
                  symbol "("
                  i <- keyLessIdentifier
                  symbol ")"
                  return (CmdGetInt i)

printIntParser :: Parser Command
printIntParser = do symbol "printint"
                    symbol "("
                    e <- expr
                    symbol ")"
                    return (CmdPrintInt e)

beginParser :: Parser Command
beginParser = do symbol "begin"
                 cs <- commandsParser
                 symbol "end"
                 return (CmdBegin cs)

commandsParser :: Parser [Command]
commandsParser = do c <- commandParser
                    do symbol ";"
                       cs <- commandsParser
                       return (c:cs)
                       <|>
                       return [c]

-- TODO create identifier that stop keywords from being parsed
keyLessIdentifier :: Parser String
keyLessIdentifier = identifier

-- expression

expr :: Parser Expr
expr = do b <- bexp
          do symbol "?"
             e0 <- bexp
             symbol ":"
             e1 <- bexp
             return (Ternary b e0 e1)
             <|>
             return b

bexp :: Parser Expr
bexp = do e0 <- cexp
          (do symbol "||"
              e1 <- bexp
              return (BinOp Disjunction e0 e1)
           <|>
           return e0)

cexp :: Parser Expr
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

bterm :: Parser Expr
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

aexp :: Parser Expr
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

mexp :: Parser Expr
mexp = mexp' id

mexp' :: (AST -> AST) -> Parser AST
mexp' f = do e0 <- aterm
             (do op <- multdiv
                 mexp' (BinOp op (f e0))
              <|>
              return (f e0))

aterm :: Parser Expr
aterm = (natural >>= return . LitInteger)
        <|> (do symbol "-"
                e <- aterm
                return (UnOp Negation e))
        <|> (do symbol "!"
                b <- aterm
                return (UnOp NegBool b))
        <|> parens expr
