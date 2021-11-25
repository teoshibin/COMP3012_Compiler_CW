module MTParser where

import FunParser
import Control.Applicative

data AST 
    = Program [Declaration] Command
    deriving (Eq,Show)

data Declaration
    = Var String Expr
    deriving (Eq,Show)

data Command 
    = CmdAssign String Expr
    | CmdIf Expr Command Command
    | CmdWhile Expr Command
    | CmdGetInt String
    | CmdPrintInt Expr
    | CmdBegin [Command]
    deriving (Eq,Show)

data Expr 
    = DeclaredVar String
    | LitInteger Int
    | BinOp BinOperator Expr Expr
    | UnOp  UnOperator Expr
    | TernaryIf Expr Expr Expr
    deriving (Eq,Show)

data UnOperator 
    = Negation 
    | NegBool
    deriving (Eq,Show)

data BinOperator
    -- arithmetic operator
    = Addition 
    | Subtraction 
    | Multiplication 
    | Division
    -- logical operator
    | Conjunction 
    | Disjunction
    -- condition operator
    | LssOp 
    | LeqOp 
    | GtrOp 
    | GeqOp 
    | EqOp 
    | NeqOp
    deriving (Eq,Show,Enum)

{-
NOTE ast definition from lecturer

type identifier = String

data Com = Assignment Identifier Expr
         | ifThenElse Expr Com Com
         | WhileDo Expr Com
         | GentInt Identifier
         | PrintInt Expr
         | BeginEnd Coms

type Coms = [Com]

data Coms = SingleC Com
          | MultipleC Com Coms
-}

keywordStrings :: [String]
keywordStrings =  [ "let"
                  , "in"
                  , "var"
                  , "if"
                  , "then"
                  , "else"
                  , "while"
                  , "do"
                  , "getint"
                  , "printint"
                  , "begin"
                  , "end"
                  ]

-- parse mini triangle
mtParse :: String -> AST
mtParse src = case parse programParser src of
    [(t, "")] -> t
    a -> error ("Parser Error:\n" ++ show a)

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

    expr ::= bexp | bexp ? bexp : bexp

    bexp ::= cexp | cexp || bexp
    cexp ::= bterm | bterm && cexp
    bterm ::= aexp | aexp `op` aexp
                where `op` is one of <,<=,>,>=,==,!=

    aexp ::= mexp | mexp + aexp | mexp - aexp
    mexp ::= aterm | aterm * mexp | aterm / mexp
    aterm ::= intLit | - aterm | ! aterm | ( exp ) | identifier

-}

-- parse entire program
programParser :: Parser AST
programParser = do  symbol "let"
                    ds <- declarationsParser
                    symbol "in"
                    c <- commandParser
                    return (Program ds c)

-- parse one declaration
declarationParser :: Parser Declaration
declarationParser = do  symbol "var"
                        i <- varNameParser
                        do  symbol ":="
                            e <- expr
                            return (Var i e)
                            <|>
                            return (Var i (LitInteger 0)) -- init declaration without expression to 0

-- parse multiple declarations
declarationsParser :: Parser [Declaration]
declarationsParser = do d <- declarationParser
                        do  symbol ";"
                            ds <- declarationsParser
                            return (d:ds)
                            <|>
                            return [d]

-- parse all types of Command
commandParser :: Parser Command
commandParser = do ifParser
            <|> do whileParser
            <|> do getIntParser
            <|> do printIntParser
            <|> do beginParser
            <|> do assignParser -- this must be the last if keywords can be used as var names

-- variable assignment
assignParser :: Parser Command
assignParser =  do  i <- varNameParser
                    symbol ":="
                    e <- expr
                    return (CmdAssign i e)

-- if condition parser
ifParser :: Parser Command
ifParser =  do  symbol "if"
                e <- expr
                symbol "then"
                c0 <- commandParser
                symbol "else"
                c1 <- commandParser
                return (CmdIf e c0 c1)

-- while loop parser
whileParser :: Parser Command
whileParser =   do  symbol "while"
                    e <- expr
                    symbol "do"
                    c <- commandParser
                    return (CmdWhile e c)
 
-- get var name for future assignment of IO value
getIntParser :: Parser Command
getIntParser =  do  symbol "getint"
                    symbol "("
                    i <- varNameParser
                    symbol ")"
                    return (CmdGetInt i)

-- get expression for future IO output
printIntParser :: Parser Command
printIntParser = do symbol "printint"
                    symbol "("
                    e <- expr
                    symbol ")"
                    return (CmdPrintInt e)

-- multi lines command parser
beginParser :: Parser Command
beginParser =   do  symbol "begin"
                    cs <- commandsParser
                    symbol "end"
                    return (CmdBegin cs)

commandsParser :: Parser [Command]
commandsParser = do c <- commandParser
                    do  symbol ";"
                        cs <- commandsParser
                        return (c:cs)
                        <|>
                        return [c]

-- identifier with addons
varNameParser :: Parser String
varNameParser = do  i <- identifier
                    if isValidName i then return i else empty

isValidName :: String -> Bool
isValidName n 
    | n `elem` keywordStrings = False
    | otherwise = True

isDeclaredName :: String -> [String] -> Bool
isDeclaredName s ls
    | s `elem` ls = False
    | otherwise = True

-- expression

expr :: Parser Expr
expr =  do  b <- bexp
            do  symbol "?"
                e0 <- bexp
                symbol ":"
                e1 <- bexp
                return (TernaryIf b e0 e1)
                <|>
                return b

bexp :: Parser Expr
bexp =  do  e0 <- cexp
            do  symbol "||"
                e1 <- bexp
                return (BinOp Disjunction e0 e1)
                <|>
                return e0

cexp :: Parser Expr
cexp = do e0 <- bterm
          do symbol "&&"
             e1 <- cexp
             return (BinOp Conjunction e0 e1)
             <|>
             return e0

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
           do op <- relop
              e1 <- aexp
              return (BinOp op e0 e1)
              <|>
              return e0


addminus :: Parser BinOperator
addminus = choice [ symbol "+" >> return Addition
                  , symbol "-" >> return Subtraction
                  ]

-- For left-associativity, we use an auxiliary function aexp'
--    that keeps a functional accumulator

aexp :: Parser Expr
aexp = aexp' id

aexp' :: (Expr -> Expr) -> Parser Expr
aexp' f = do e0 <- mexp
             do op <- addminus
                aexp' (BinOp op (f e0))
                <|>
                return (f e0)

multdiv :: Parser BinOperator
multdiv = choice [ symbol "*" >> return Multiplication
                 , symbol "/" >> return Division
                 ]

mexp :: Parser Expr
mexp = mexp' id

mexp' :: (Expr -> Expr) -> Parser Expr
mexp' f =   do  e0 <- aterm
                do  op <- multdiv
                    mexp' (BinOp op (f e0))
                    <|>
                    return (f e0)

-- TODO identify var name is declared
aterm :: Parser Expr
aterm = (natural >>= return . LitInteger)
        <|> (do symbol "-"
                e <- aterm
                return (UnOp Negation e))
        <|> (do symbol "!"
                b <- aterm
                return (UnOp NegBool b))
        <|> parens expr
        <|> (do i <- varNameParser
                return (DeclaredVar i))
