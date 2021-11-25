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

placeHolderInt = 9999
placeHolderString = "#placeHolder"

{- 
    CONVERT AST TO TAM 
    calling other conversion method and append 'HALT' at the end of the program
-}
convertAST :: AST -> [TAMInst]
convertAST (Program ds c) = 
    convertDeclarations ds ++
    convertCommand c ++
    [HALT]


{- 
    CONVERT LIST OF DECLARATIONS
    apply convertDeclaration to each element then concat all of them into a list
-}
convertDeclarations :: [Declaration] -> [TAMInst]
-- convertDeclarations [] = []
-- convertDeclarations (d:ds) = convertDeclaration d ++ convertDeclarations ds
convertDeclarations = concatMap convertDeclaration


{- 
    CONVERT SINGLE DECLARATIONS
    TODO turn variable name into address
-}
convertDeclaration :: Declaration -> [TAMInst]
convertDeclaration (Var s (LitInteger n)) = [LOADL n]
convertDeclaration (Var s e) = convertExpr e


{- 
    CONVERT COMMAND
    TODO label
    TODO address
-}
convertCommand :: Command -> [TAMInst]

-- ASSIGNMENT
convertCommand (CmdAssign _ (LitInteger n)) = 
    [LOADL n] ++
    [STORE placeHolderInt]

convertCommand (CmdAssign _ e) = 
    convertExpr e ++
    [STORE placeHolderInt]

-- IF CONDITION
convertCommand (CmdIf e c1 c2) =
    convertExpr e ++ 
    [JUMPIFZ placeHolderString] ++
    convertCommand c1 ++
    [JUMP placeHolderString] ++
    [LABEL placeHolderString] ++
    convertCommand c2 ++
    [LABEL placeHolderString]

-- WHILE LOOP
convertCommand (CmdWhile e c) =
    [LABEL placeHolderString] ++
    convertExpr e ++
    [JUMPIFZ placeHolderString] ++
    convertCommand c ++
    [JUMP placeHolderString] ++
    [LABEL placeHolderString]

-- GETINT
convertCommand (CmdGetInt s) = 
    [GETINT] ++
    [STORE placeHolderInt]

-- PRINTINT
convertCommand (CmdPrintInt e) =
    convertExpr e ++
    [PUTINT]

-- commands
convertCommand (CmdBegin []) = []
convertCommand (CmdBegin (c:cs)) =
    convertCommand c ++
    convertCommand (CmdBegin cs)

{- 
    CONVERT EXPRESSIONS
    TODO address
-}
convertExpr :: Expr -> [TAMInst]

convertExpr (LitInteger x) = [LOADL x]

convertExpr (DeclaredVar s) = [LOAD placeHolderInt] 

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
convertExpr (BinOp op t1 t2) =
    convertExpr t1 ++
    convertExpr t2 ++
    [binOpTAM op]

convertExpr (UnOp op t) =
    convertExpr t ++
    [unOpTAM op]

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
