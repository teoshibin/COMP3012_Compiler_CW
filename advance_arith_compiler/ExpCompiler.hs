{-

Compilers Course (COMP3012), 2021
  Venanzio Capretta
  Nicolai Kraus

A Compiler and evaluator for Arithmetic expressions with booleans,
relations, and conditional expressions.
Compiles simple Arith Expr into TAM programs.

-}

module ExpCompiler where

import TAM
import ExpParser
import ExpTAM

-- Directly evaluate the input expression

eval :: String -> MTInt
eval = evaluate . expParse

-- Evaluator for AST

evaluate :: AST -> MTInt
evaluate (LitInteger x)   = x
evaluate (BinOp op t1 t2) = binOpEv op (evaluate t1) (evaluate t2)
evaluate (UnOp op t)      = unOpEv op (evaluate t)
evaluate (Conditional b t1 t2) = if (evaluate b) /= 0
                                 then (evaluate t1)
                                 else (evaluate t2)

binOpEv :: BinOperator -> MTInt -> MTInt -> MTInt
binOpEv Addition       = (+)
binOpEv Subtraction    = (-)
binOpEv Multiplication = (*)
binOpEv Division       = div
binOpEv Conjunction    = intAND
binOpEv Disjunction    = intOR
binOpEv LssOp          = relInt (<)
binOpEv LeqOp          = relInt (<=)
binOpEv GtrOp          = relInt (>)
binOpEv GeqOp          = relInt (>=)
binOpEv EqOp           = relInt (==)
binOpEv NeqOp          = relInt (/=)

-- Boolean relation that returns an integer
relInt :: (MTInt -> MTInt -> Bool) -> MTInt -> MTInt -> MTInt
relInt rel = boolInt .< rel

unOpEv :: UnOperator -> MTInt -> MTInt
unOpEv Negation = negate
unOpEv NegBool  = intNOT

-- Compiling Arithmetic Expressions to TAM

compArith :: String -> [TAMInst]
compArith = expCode . expParse

-- reading from a file

compileArithTAM :: String -> String
compileArithTAM = writeTAM . compArith

