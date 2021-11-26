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
import StateC

-- placeHolderInt = 9999
-- placeHolderString = "#placeHolder"

{- 
    VARIABLE ENVIRONMENT
-}

type VarEnv = [(Identifier, StackAddress)]

getAddress :: VarEnv -> Identifier -> StackAddress
getAddress ve v = case lookup v ve of
    Nothing -> error "Variable not declared"
    Just a -> a

{- 
    CONVERT AST TO TAM 
    calling other conversion method and append 'HALT' at the end of the program
-}
convertAST :: AST -> [TAMInst]
convertAST (Program ds c) =
    let (ve, ts) = declResult ds
    in ts ++ commResult ve c ++ [HALT]


-- Finally, what we actually want:
declResult :: [Declaration] -> (VarEnv,[TAMInst])
declResult ds =
    let (ts,(ve,a)) = app (convertDeclarations ds) ([],0)
    -- ([],0) is the initial state!
    in (ve,ts)

{- 
    CONVERT LIST OF DECLARATIONS
    apply convertDeclaration to each element then concat all of them into a list
-}
convertDeclarations :: [Declaration] -> ST (VarEnv,StackAddress) [TAMInst]
convertDeclarations [] = return []
convertDeclarations (d:ds) = do
    t <- convertDeclaration d
    ts <- convertDeclarations ds
    return (t ++ ts)


{- 
    CONVERT SINGLE DECLARATIONS
-}
convertDeclaration :: Declaration -> ST (VarEnv,StackAddress) [TAMInst]
convertDeclaration (EmptyVar i) = do
    (ve,a) <- stGet
    stUpdate ((i,a):ve,a+1)
    return
        [LOADL 0]

convertDeclaration (Var i e) = do
    (ve,a) <- stGet
    stUpdate ((i,a):ve,a+1)
    return (convertExpr ve e)

{- 
    CONVERT COMMAND
-}

fresh :: ST Int LabelName
fresh = do n <- stGet
           stUpdate(n+1)
           return ('#' : (show n))

commResult :: VarEnv -> Command -> [TAMInst]
commResult ve c =
    let (ts,_) = app (convertCommand ve c) 0
    in ts

convertCommand :: VarEnv -> Command -> ST Int [TAMInst]

-- ASSIGNMENT
convertCommand ve (CmdAssign i (LitInteger n)) =
    return (
        [LOADL n] ++
        [STORE (getAddress ve i)]
        )

convertCommand ve (CmdAssign i e) =
    return (
        convertExpr ve e ++
        [STORE (getAddress ve i)]
    )

-- IF CONDITION
convertCommand ve (CmdIf e c1 c2) = do
    l1 <- fresh
    l2 <- fresh
    com1 <- convertCommand ve c1
    com2 <- convertCommand ve c2
    return (
        convertExpr ve e ++
        [JUMPIFZ l1] ++
        com1 ++
        [JUMP l2] ++
        [Label l1] ++
        com2 ++
        [Label l2]
        )

-- WHILE LOOP
convertCommand ve (CmdWhile e c) = do
    l1 <- fresh
    l2 <- fresh
    com1 <- convertCommand ve c
    return (
        [Label l1] ++
        convertExpr ve e ++
        [JUMPIFZ l2] ++
        com1 ++
        [JUMP l1] ++
        [Label l2]
        )

-- GETINT
convertCommand ve (CmdGetInt i) =
    return (
        [GETINT] ++
        [STORE (getAddress ve i)]
        )


-- PRINTINT
convertCommand ve (CmdPrintInt e) =
    return (
        convertExpr ve e ++
        [PUTINT]
        )

-- commands
convertCommand ve (CmdBegin []) = return []
convertCommand ve (CmdBegin (c:cs)) = do
    com1 <- convertCommand ve c
    com2 <- convertCommand ve (CmdBegin cs)
    return (com1 ++ com2)


{- 
    CONVERT EXPRESSIONS
-}
convertExpr :: VarEnv -> Expr -> [TAMInst]

convertExpr ve (LitInteger x) = [LOADL x]

convertExpr ve (DeclaredVar i) = [LOAD (getAddress ve i)]

-- Relational operators that don't have TAM instructions
convertExpr ve (BinOp LeqOp t1 t2) =
    convertExpr ve (BinOp Disjunction (BinOp LssOp t1 t2) (BinOp EqOp t1 t2))
convertExpr ve (BinOp GeqOp t1 t2) =
    convertExpr ve (BinOp Disjunction (BinOp GtrOp t1 t2) (BinOp EqOp t1 t2))
convertExpr ve (BinOp NeqOp t1 t2) =
    convertExpr ve (UnOp NegBool (BinOp EqOp t1 t2))

-- Conditional expressions (double negation to normalize the Boolean value)
--   b ? e1 : e2  ~  (!!b) * e1 + (!b) * e2
convertExpr ve (TernaryIf b t1 t2) =
    convertExpr ve (BinOp Addition
               (BinOp Multiplication (UnOp NegBool (UnOp NegBool b)) t1)
               (BinOp Multiplication (UnOp NegBool b) t2)
            )

-- General cases
convertExpr ve (BinOp op t1 t2) =
    (convertExpr ve t1 ++ convertExpr ve t2 ++ [binOpTAM op])

convertExpr ve (UnOp op t) =
    (convertExpr ve t ++ [unOpTAM op])

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
