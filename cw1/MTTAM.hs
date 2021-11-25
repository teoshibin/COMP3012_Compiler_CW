{-

Compilers Course (COMP3012), 2021
  Venanzio Capretta
  Nicolai Kraus

Generation of TAM code from the AST of an Arithmetic expression
Extension with Boolean, Relational, Conditional Expressions

-}

module MTTAM where

import MTParser
import TAM

convertAST :: AST -> [TAMInst]
convertAST (Program ds c) = convertDeclarations ds ++ convertCommand c

convertDeclarations :: [Declaration] -> [TAMInst]
convertDeclarations = concatMap convertDeclaration
-- convertDeclarations [] = []
-- convertDeclarations (d:ds) = convertDeclaration d ++ convertDeclarations ds

convertDeclaration :: Declaration -> [TAMInst]
convertDeclaration (Var _ (LitInteger n)) = [LOADL n]
convertDeclaration (Var _ e) = convertExpr e

convertCommand :: Command -> [TAMInst]
convertCommand (CmdAssign s e) = undefined

convertExpr :: Expr -> [TAMInst]
convertExpr (LitInteger x) = [LOADL x]
-- Relational operators that don't have TAM instructions
convertExpr (BinOp LeqOp t1 t2) =
    convertExpr (BinOp Disjunction (BinOp LssOp t1 t2) (BinOp EqOp t1 t2))
convertExpr (BinOp GeqOp t1 t2) =
    convertExpr (BinOp Disjunction (BinOp GtrOp t1 t2) (BinOp EqOp t1 t2))
convertExpr (BinOp NeqOp t1 t2) =
    convertExpr (UnOp NegBool (BinOp EqOp t1 t2))
-- Conditional expressions (double negation to normalize the Boolean value)
--   b ? e1 : e2  ~  (!!b) * e1 + (!b) * e2
convertExpr (TernaryIf b t1 t2) =
    convertExpr (BinOp Addition
               (BinOp Multiplication (UnOp NegBool (UnOp NegBool b)) t1)
               (BinOp Multiplication (UnOp NegBool b) t2)
            )
-- General cases
convertExpr (BinOp op t1 t2) = convertExpr t1 ++ convertExpr t2 ++ [binOpTAM op]
convertExpr (UnOp op t) = convertExpr t ++ [unOpTAM op]

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
