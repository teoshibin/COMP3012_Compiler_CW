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
  parse "32*  9/ 3 + 5"  [Number 32,Operator Times,Number 9,Operator Divide,Number 3,Operator Plus,Number 5]
the result should be:
  BinOp Addition (BinOp Multiplication (LitInteger 32) (BinOp Division (LitInteger 9) (LitInteger 3))) (LitInteger 5).

Of course, `parse` will make use of the scanner. You can
use the following definition:
-}

parse :: String -> AST
parse s =
  case parseExp (scan s) of
    Nothing -> error "Parser: Failed to parse expression"
    Just (ast,ts) -> if null ts
                     then ast
                     else error "Parser: Unused tokens"

{-
You can solve this exercise by replacing the occurrences
of "undefined" by the correct Haskell code, or, if you
prefer, delete the below lines and do it from scratch.

     exp ::= mexp | mexp + exp | mexp - exp
     mexp ::= term | term * mexp | term / mexp
     term ::= int | - term | ( exp )

-}

parseExp :: [Token] -> Maybe (AST, [Token])
parseExp = undefined


parseMExp :: [Token] -> Maybe (AST, [Token])
parseMExp = undefined

parseTerm :: [Token] -> Maybe (AST, [Token])
parseTerm ts =
  case parseIntLit ts of
    Just(n, ts) -> Just(LitInteger n, ts)
    Nothing -> case perseTerm ts of
      Just(Operator Minus : ts) -> Just(Negation, parseTerm ts)
      Nothing -> Nothing




parseIntLit :: [Token] -> Maybe (Int, [Token])
parseIntLit (t:ts) = case t of
    Number n -> Just(n, ts)
    _ -> Nothing

parseBinOp :: [Token] -> Maybe (BinOperator,[Token])
parseBinOp (Operator t:ts) = case t of
    Plus -> Just(Addition, ts)
    Minus -> Just(Subtraction, ts)
    Divide -> Just(Division, ts)
    Times -> Just(Multiplication, ts)
parseBinOp _ = Nothing

parseUnOp :: [Token] -> Maybe(UnOperator, [Token])
parseUnOp (Operator Minus : ts) = Just(Negation, ts)
parseUnOp _ = Nothing

{- If all of the above works: Can you extend your code
such that parentheses can be parsed as well?
-}