{-
Compilers (COMP3012)
University of Nottingham, 20 Oct 2021
Nicolai Kraus (replacing Venanzio Capretta)
-}

module ArithParser where
import ArithScanner

{-
Recall the language Arith, given by the following BNF:

     exp ::= mexp | mexp + exp | mexp - exp
     mexp ::= term | term * mexp | term / mexp
     term ::= int | - term | ( exp )

We have written a scanner for this language, imported
above. The next goal is to write a parser. Recall that
a parser takes a string of tokens (produced by the
scanner) and produces an abstract syntax tree.

We first define the datatype AST of such syntax trees.
This could be a single definition, but it's more
elegant to present it as follows:
-}

data BinOperator = Addition | Subtraction | Multiplication | Division
  deriving (Eq,Show)

data UnOperator = Negation
  deriving (Eq,Show)

data AST = LitInteger Int
         | BinOp BinOperator AST AST
         | UnOp  UnOperator AST
  deriving (Eq,Show)

{-
Your exercise is to write the parser. For the beginning,
it is okay to only parse expressions without parentheses,
i.e. you can assume that the expression does not contain
any `(` and `)`.

The final parse function should be of type
  parse :: String -> AST.
If you call
  parse "32*  9/ 3 + 5"
the result should be:
  BinOp Addition (BinOp Multiplication (LitInteger 32) (BinOp Division (LitInteger 9) (LitInteger 3))) (LitInteger 5).

Of course, `parse` will make use of the scanner. You can
use the following definition:
-}

parse :: String -> AST
parse s =
  case parseExp (scan s) of
    Nothing -> error "Parser: Failed to parse expression"
    Just (ast,ts) -> if ts == []
                     then ast
                     else error "Parser: Unused tokens"

{-
You can solve this exercise by replacing the occurrences
of "undefined" by the correct Haskell code, or, if you
prefer, delete the below lines and do it from scratch.
-}

{-
parseExp :: [Token] -> Maybe (AST, [Token])
parseExp = undefined

parseMExp :: [Token] -> Maybe (AST, [Token])
parseMExp = undefined

parseTerm :: [Token] -> Maybe (AST, [Token])
parseTerm = undefined

parseIntLit :: [Token] -> Maybe (Int, [Token])
parseIntLit = undefined

parseBinOp :: [Token] -> Maybe (BinOperator,[Token])
parseBinOp = undefined
-}

-- SOLUTIONS (the following handles parentheses):

parseExp :: [Token] -> Maybe (AST, [Token])
parseExp ts = case parseMExp ts of
  Nothing -> Nothing
  Just (mexp,ts') -> case parsePlusOrMinus ts' of
    Nothing -> Just (mexp,ts')
    Just (o,ts2) -> case parseExp ts2 of
      Nothing -> Nothing
      Just (mexp',ts3) -> Just (BinOp o mexp mexp', ts3)

parseMExp :: [Token] -> Maybe (AST, [Token])
parseMExp ts = case parseTerm ts of
  Nothing -> Nothing
  Just (t, ts') -> case parseTimesOrDivide ts' of
    Nothing -> Just (t,ts')
    Just (o,ts2) -> case parseMExp ts2 of
      Nothing -> Nothing
      Just (t',ts3) -> Just (BinOp o t t', ts3)

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
            Just ts3 -> Just (UnOp Negation e, ts3)
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

{- NOTE:

The above parser re-constructs the derivation of an expression
as give by the BNF. Unfortunately, this BNF causes all
arithmetic operations to be implicitly RIGHT-associative; for
example, "3-4+5" is parsed as 3 - (4 + 5) and "8/2*4"
as 8 / (2*4). However, arithmetic operators are usually
understood to be LEFT-associative, meaning that "3-4+5"
should be read as "(3-4)+5" and "8/2*4" as (8/2)*4.

To achieve this, we can write the BNF as

     exp ::= mexp | exp + mexp | exp - mexp
     mexp ::= term | mexp * term | mexp / term
     term ::= int | - term | ( exp )

Writing a parser is now a bit trickier. The natural way
would now be to parse "from the right"; to do this, one
could, for example, start by reversing the token list.

We will continue to use the version of the language
where operators are right-associative.
-}

