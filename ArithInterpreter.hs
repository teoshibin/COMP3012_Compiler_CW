module ArithInterpreter where
import ArithParser

evaluate :: AST -> Int
evaluate (LitInteger n) = n
evaluate (UnOp Negation n) = - evaluate n
evaluate (BinOp op x y) = binOpeval op (evaluate x) (evaluate y)

binOpeval :: BinOperator -> Int -> Int -> Int
binOpeval op
    | op == Addition = (+)
    | op == Subtraction = (-)
    | op == Multiplication = (*)
    | op == Division = div

interpret :: String -> Int
interpret = evaluate . parse
