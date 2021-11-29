{-

Compilers Course (COMP3012), 2021
    Venanzio Capretta
    Nicolai Kraus

Additional Code written by
    Shi Bin Teo
    
-}

{-
    Evaluator for MT
-}

module Interpreter where

--INTERPRETER
-- TODO interpreter
-- Directly evaluate the input expression

-- eval :: String -> TAMInt
-- eval = evaluate . mtParse

-- -- Evaluator for AST

-- evaluate :: Expr -> TAMInt
-- evaluate (LitInteger x)   = x
-- evaluate (BinOp op t1 t2) = binOpEv op (evaluate t1) (evaluate t2)
-- evaluate (UnOp op t)      = unOpEv op (evaluate t)
-- evaluate (TernaryIf b t1 t2) =
--     if (evaluate b) /= 0
--         then (evaluate t1)
--         else (evaluate t2)

-- binOpEv :: BinOperator -> TAMInt -> TAMInt -> TAMInt
-- binOpEv Addition       = (+)
-- binOpEv Subtraction    = (-)
-- binOpEv Multiplication = (*)
-- binOpEv Division       = div
-- binOpEv Conjunction    = intAND
-- binOpEv Disjunction    = intOR
-- binOpEv LssOp          = relInt (<)
-- binOpEv LeqOp          = relInt (<=)
-- binOpEv GtrOp          = relInt (>)
-- binOpEv GeqOp          = relInt (>=)
-- binOpEv EqOp           = relInt (==)
-- binOpEv NeqOp          = relInt (/=)

-- -- Boolean relation that returns an integer
-- relInt :: (TAMInt -> TAMInt -> Bool) -> TAMInt -> TAMInt -> TAMInt
-- relInt rel = boolInt .< rel

-- unOpEv :: UnOperator -> TAMInt -> TAMInt
-- unOpEv Negation = negate
-- unOpEv NegBool  = intNOT
