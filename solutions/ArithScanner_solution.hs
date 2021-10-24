{-
          Compilers (COMP3012)
  University of Nottingham, 18 Oct 2021
Nicolai Kraus (replacing Venanzio Capretta)

Today: Writing a scanner for the language Arith
of arithmetic expression, e.g. "32 + 8/1".

Goal for today: Write scanner (and start with parser?)
Goal for Lab on Wednesday: complete both.
-}

-- SOLUTION: The lines marked with *NEW*
-- and *MODIFIED* add support for ( and )

module ArithScanner where
import Data.Char

data Op = Plus | Minus | Times | Divide
  deriving (Eq,Show)

data Par = Open | Closed -- *NEW*
  deriving (Eq,Show) -- *NEW*

data Token = Number Int | Operator Op | Parenthesis Par -- *MODIFIED*
  deriving (Eq,Show)

scan :: String -> [Token]
scan "" = []
scan (c:s') | isSpace c = scan s'
scan s =
  case scanToken s of
    Nothing -> error "scanner: No token found although string not empty"
    Just (t,s') -> t : scan s'

scanNumber :: String -> Maybe (Int,String)
scanNumber s =
  let (n,s') = span isDigit s
  in  if n=="" then Nothing
               else Just (read n, s')

scanOperator :: String -> Maybe (Op, String)
scanOperator "" = Nothing
scanOperator (o:s') = case o of
  '+' -> Just (Plus, s')
  '-' -> Just (Minus, s')
  '*' -> Just (Times, s')
  '/' -> Just (Divide, s')
  _   -> Nothing

scanPar :: String -> Maybe (Par, String) -- *NEW*
scanPar ('(':s) = Just (Open, s) -- *NEW*
scanPar (')':s) = Just (Closed, s) -- *NEW*
scanPar _ = Nothing -- *NEW*

scanToken :: String -> Maybe (Token, String)
scanToken s =
  case scanNumber s of
    Just (n,s') -> Just (Number n, s')
    Nothing -> case scanOperator s of
      Just (o,s') -> Just (Operator o, s')
      Nothing -> case scanPar s of -- *MODIFIED*
        Just (x,s') -> Just (Parenthesis x, s') -- *NEW*
        Nothing -> Nothing -- *NEW*
