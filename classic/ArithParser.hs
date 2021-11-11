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

-- full parse
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

-- Exp
parseExp :: [Token] -> Maybe (AST, [Token])
parseExp ts = case parseMExp ts of
  Nothing -> Nothing
  Just(a, ts1) -> case parsePlusOrMinus ts1 of
    Nothing -> Just(a, ts1)
    Just(b, ts2) -> case parseExp ts2 of
      Nothing -> Nothing
      Just(c, ts3) -> Just(BinOp b a c, ts3)

-- Mexp
parseMExp :: [Token] -> Maybe (AST, [Token])
parseMExp ts = case parseTerm ts of
  Nothing -> Nothing
  Just(a, ts1) -> case parseTimesOrDivide ts1 of
    Nothing -> Just(a, ts1) -- even though this fail but it can still be term so return term
    Just(b, ts2) -> case parseMExp ts2 of
      Nothing -> Nothing
      Just(c, ts3) -> Just (BinOp b a c, ts3)

-- Term
parseTerm :: [Token] -> Maybe (AST, [Token])
parseTerm ts =
  case parseIntLit ts of
    Just(n, ts) -> Just(LitInteger n, ts)
    Nothing -> case parseNegTerm ts of
      Just(UnOp Negation t1, ts1) -> Just(UnOp Negation t1, ts1)
      Nothing -> case parseParenExp ts of
        Just(t2, ts2) -> Just(t2, ts2)
        Nothing -> Nothing

-- parse BinOperator
parseBinOp :: [Token] -> Maybe (BinOperator,[Token])
parseBinOp (Operator t:ts) = case t of
    Plus -> Just(Addition, ts)
    Minus -> Just(Subtraction, ts)
    Divide -> Just(Division, ts)
    Times -> Just(Multiplication, ts)
parseBinOp _ = Nothing

-- parse BinOperator if it is plus or minus
parsePlusOrMinus :: [Token] -> Maybe (BinOperator, [Token])
parsePlusOrMinus ts = case parseBinOp ts of
  Nothing -> Nothing
  Just(a, ts) -> if a == Addition || a == Subtraction
    then Just(a, ts)
    else Nothing

-- parse BinOperator if it is times or divide
parseTimesOrDivide :: [Token] -> Maybe (BinOperator, [Token])
parseTimesOrDivide ts = case parseBinOp ts of
  Nothing -> Nothing
  Just(a, ts1) -> if a == Multiplication || a == Division
    then Just(a , ts1)
    else Nothing

-- parse number (parseTerm)
parseIntLit :: [Token] -> Maybe (Int, [Token])
parseIntLit (Number n : ts) = Just(n, ts)
parseIntLit _ = Nothing

-- parse negative term (parseTerm)
parseNegTerm :: [Token] -> Maybe (AST, [Token])
parseNegTerm ts = case parseUnOp ts of
  Nothing -> Nothing
  Just(Negation, ts') -> case parseTerm ts' of
    Nothing -> Nothing
    Just(t, ts1) -> Just(UnOp Negation t, ts1)

-- parse parenthesis sindwiched expression (parseTerm)
parseParenExp :: [Token] -> Maybe (AST, [Token])
parseParenExp ts = case parseOpenPar ts of
  Nothing -> Nothing
  Just ts' -> case parseExp ts' of
    Nothing -> Nothing
    Just(t, ts1) -> case parseClosedPar ts1 of
      Nothing -> Nothing
      Just ts2 -> Just(t, ts2)

-- parse minus token (parseNegTerm)
parseUnOp :: [Token] -> Maybe(UnOperator, [Token])
parseUnOp (Operator Minus : ts) = Just(Negation, ts)
parseUnOp _ = Nothing

-- parse open parenthesis (parseParenExp)
parseOpenPar :: [Token] -> Maybe[Token]
parseOpenPar (t:ts) = case t of
  Parenthesis Open -> Just ts
  _ -> Nothing

-- parse closed parenthesis (parseParenExp)
parseClosedPar :: [Token] -> Maybe[Token]
parseClosedPar (t:ts) = case t of
  Parenthesis Closed -> Just ts
  _ -> Nothing

{- If all of the above works: Can you extend your code
such that parentheses can be parsed as well?
-}