{-
Compilers (COMP3012)
University of Nottingham, 20 Oct 2021
Nicolai Kraus (replacing Venanzio Capretta)

The goal of this file is to improve the parser
from last week by making binary operators
LEFT-ASSOCIATIVE:
7 - 5 + 3 was, with the previous parser, parsed
as 7 - (5 + 3), but this is not the usual
meaning of the expression. Thus, we instead
want to parse it as (7 - 5) + 3.

This file contains my (Nicolai's) solution.
I'm curious what solutions you have come up with!
-}

module ArithLeftAssocParser where
import ArithScanner

{-
Recall the language Arith, given by the following BNF:

     exp ::= mexp | mexp + exp | mexp - exp
     mexp ::= term | term * mexp | term / mexp
     term ::= int | - term | ( exp )

We have written a scanner for this language, imported
above.

We first define the datatype AST of such syntax trees.
-}

data BinOperator =
  Addition | Subtraction | Multiplication | Division
  deriving (Eq,Show)

data UnOperator = Negation
  deriving (Eq,Show)

data AST = LitInteger Int
         | BinOp BinOperator AST AST
         | UnOp  UnOperator AST
  deriving (Eq,Show)

-- parsing everything
parse :: String -> AST
parse s =
  case parseExp (scan s) of
    Nothing -> error "Parser: Failed to parse expression"
    Just (ast,ts) -> if ts == []
                     then ast
                     else error "Parser: Unused tokens"

-- turn a list of the form [a,+,b,+,c,-,d,-,e],
-- here represented as AST , [(BinOperator,AST)],
-- into a single AST
assemble :: AST -> [(BinOperator,AST)] -> AST
assemble first [] = first
assemble first ((op,second):rest) =
  assemble (BinOp op first second) rest

-- parsing an expression
parseExp :: [Token] -> Maybe (AST, [Token])
parseExp ts = case parseMExp ts of
  Nothing -> Nothing
  Just (mexp,ts2) ->
    let (mexps,ts3) = parseMExpList ts2
    in Just ((assemble mexp mexps) , ts3)

-- parsing a multiplicative expression
parseMExp :: [Token] -> Maybe (AST, [Token])
parseMExp ts = case parseTerm ts of
  Nothing -> Nothing
  Just (ter, ts2) ->
    let (ters,ts3) = parseTermList ts2
    in Just ((assemble ter ters) , ts3)

-- parsing a LIST of multiplicative expressions
parseMExpList :: [Token] -> ([(BinOperator,AST)],[Token])
parseMExpList ts =
  case parsePlusOrMinus ts of
    Nothing -> ([],ts)
    Just (x,ts1) -> case parseMExp ts1 of
      Nothing -> error "Parser: found + or - without mult. expression following"
      Just (m1,ts2) ->
        let (ms,ts3) = parseMExpList ts2
        in ((x,m1):ms , ts3)

-- parsing a LIST of terms
parseTermList :: [Token] -> ([(BinOperator,AST)],[Token])
parseTermList ts =
  case parseTimesOrDivide ts of
    Nothing -> ([],ts)
    Just (x,ts1) -> case parseTerm ts1 of
      Nothing -> error "Parser: found * or / without term following"
      Just (m1,ts2) ->
        let (ms,ts3) = parseTermList ts2
        in ((x,m1):ms , ts3)

parseTerm :: [Token] -> Maybe (AST, [Token])
parseTerm ts =
  case parseIntLit ts of
    Just (n, ts') -> Just (LitInteger n, ts')
    Nothing -> case parseUnOp ts of
      Just (Negation,ts') -> case parseTerm ts of
        Just (t,ts2) -> Just (UnOp Negation t, ts2)
        Nothing -> Nothing
      Nothing -> case parseOpenPar ts of
        Just ts' -> case parseExp ts' of
          Just (e,ts2) -> case parseClosedPar ts2 of
            Just ts3 -> Just (e, ts3)
            Nothing -> Nothing
          Nothing -> Nothing
        Nothing -> Nothing

parseIntLit :: [Token] -> Maybe (Int, [Token])
parseIntLit (Number n : ts) = Just (n, ts)
parseIntLit _ = Nothing

parseBinOp :: [Token] -> Maybe (BinOperator,[Token])
parseBinOp (Operator x : ts) =
  case x of
    Plus -> Just (Addition, ts)
    Minus -> Just (Subtraction, ts)
    Times -> Just (Multiplication, ts)
    Divide -> Just (Division, ts)
parseBinOp _ = Nothing

parseTimesOrDivide :: [Token] -> Maybe (BinOperator,[Token])
parseTimesOrDivide ts = case parseBinOp ts of
  Nothing -> Nothing
  Just (x,ts) -> if x==Multiplication || x==Division
                    then Just(x,ts)
                    else Nothing

parsePlusOrMinus :: [Token] -> Maybe (BinOperator,[Token])
parsePlusOrMinus ts = case parseBinOp ts of
  Nothing -> Nothing
  Just (x,ts) -> if x==Addition || x==Subtraction
                    then Just(x,ts)
                    else Nothing

parseUnOp :: [Token] -> Maybe (UnOperator,[Token])
parseUnOp (Operator Minus : ts) = Just (Negation,ts)
parseUnOp _ = Nothing

parseOpenPar :: [Token] -> Maybe [Token]
parseOpenPar (Parenthesis Open : ts) = Just ts
parseOpenPar _ = Nothing

parseClosedPar :: [Token] -> Maybe [Token]
parseClosedPar (Parenthesis Closed : ts) = Just ts
parseClosedPar _ = Nothing

-- Not part of the exercise sheet, but it's easy to
-- write an evaluator:

eval :: AST -> Int
eval (LitInteger n) = n
eval (BinOp o x y) =
  let
    a = eval x
    b = eval y
  in
    case o of
      Addition -> a + b
      Subtraction -> a - b
      Multiplication -> a * b
      Division -> a `div` b
eval (UnOp Negation x) = - (eval x)

compute :: String -> Int
compute = eval . parse
