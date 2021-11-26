{-

Compilers Course (COMP3012), 2021
  Venanzio Capretta
  Nicolai Kraus

Generation of TAM code from the AST of an Arithmetic expression
Extension with Boolean, Relational, Conditional Expressions

-}

module ExpTAM where

import ExpParser
import TAM

expCode :: AST -> [TAMInst]
expCode (LitInteger x) = [LOADL x]
-- Relational operators that don't have TAM instructions
expCode (BinOp LeqOp t1 t2) =
  expCode (BinOp Disjunction (BinOp LssOp t1 t2) (BinOp EqOp t1 t2))
expCode (BinOp GeqOp t1 t2) =
  expCode (BinOp Disjunction (BinOp GtrOp t1 t2) (BinOp EqOp t1 t2))
expCode (BinOp NeqOp t1 t2) =
  expCode (UnOp NegBool (BinOp EqOp t1 t2))
-- Conditional expressions (double negation to normalize the Boolean value)
--   b ? e1 : e2  ~  (!!b) * e1 + (!b) * e2
expCode (Conditional b t1 t2) =
  expCode (BinOp Addition
             (BinOp Multiplication (UnOp NegBool (UnOp NegBool b)) t1)
             (BinOp Multiplication (UnOp NegBool b) t2))
-- General cases
expCode (BinOp op t1 t2) = (expCode t1) ++ (expCode t2) ++ [binOpTAM op]
expCode (UnOp op t) = (expCode t) ++ [unOpTAM op]

binOpTAM :: BinOperator -> TAMInst
binOpTAM Addition       = ADD
binOpTAM Subtraction    = SUB
binOpTAM Multiplication = MUL
binOpTAM Division       = DIV
binOpTAM Conjunction    = AND
binOpTAM Disjunction    = OR
binOpTAM LssOp          = LSS
binOpTAM GtrOp          = GTR
binOpTAM EqOp           = EQL

unOpTAM :: UnOperator -> TAMInst
unOpTAM Negation = NEG
unOpTAM NegBool  = NOT
