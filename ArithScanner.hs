{-
Scanner the first component of compiler
It is to take in string and convert it into tokens
-}

module ArithScanner where
import Data.Char

data Op = Plus | Minus | Times | Divide
    deriving (Eq, Show)

data Par = Open | Closed
    deriving (Eq, Show)

data Token = Number Int | Operator Op | Parenthesis Par 
    deriving (Eq, Show)

-- scan Number
-- get first string of numbers and the remaining string
-- then return the read number and the remaining string
-- if condition to prevent empty string parse into read function
scanNumber :: String -> Maybe (Int, String)
scanNumber s =
    let (n, s') = span isDigit s
    in if n == "" then Nothing
                  else Just (read n, s')

-- scan operator
-- maybe is needed because tuple and Nothing are different type
-- guard / case each symbol then return tuple of operator and remaning string
scanOperator :: String -> Maybe (Op, String)
scanOperator "" = Nothing
scanOperator (o : s')
  | o == '+' = Just (Plus, s')
  | o == '-' = Just (Minus, s')
  | o == '*' = Just (Times, s')
  | o == '/' = Just (Divide, s')
  | otherwise = Nothing

-- scan parenthesis
scanParenthesis :: String -> Maybe (Par, String)
scanParenthesis "" = Nothing
scanParenthesis (p: s') 
    | p == '(' = Just (Open, s')
    | p == ')' = Just (Closed, s')
    | otherwise = Nothing

-- scan a single token
-- scan number with the input String
scanToken :: String -> Maybe (Token, String)
scanToken s = 
    case scanNumber s of
        Just (n, s') -> Just (Number n, s')
        Nothing -> case scanOperator s of
            Just (o, s') -> Just (Operator o, s')
            Nothing -> case scanParenthesis s of
                Just (p, s') -> Just (Parenthesis p, s')
                Nothing -> Nothing 

-- take in string and convert it into tokens
-- if the string is empty return empty list
-- if the character is space then ignore the space and scan the remaining
-- else scan a token with string that contain no spaces from the begining
scan :: String -> [Token]
scan "" = []
scan (c:s') | isSpace c = scan s'
scan s = 
    case scanToken s of
        Just (t, s') -> t : scan s'
        Nothing -> error "Scanner: No token found although string is not empty"