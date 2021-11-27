{-

Compilers Course (COMP3012), 2021
    Venanzio Capretta
    Nicolai Kraus

Additional Code written by
    Shi Bin Teo

-}

{-
    MiniTriangle AST to TAM
    
    Generator for TAM code
-}

module MTTAM where

import MTParser
import TAM
import StateC



-- ----------------------------- Data Structure ----------------------------- --

{- 
    variable environment state 
-}
type VarEnv = [(Identifier, StackAddress)]



-- ----------------------- TAM Instructions Generator ----------------------- --

{- 
    VARIABLE ENVIRONMENT
-}

getAddress :: VarEnv -> Identifier -> Maybe StackAddress
getAddress ve v = lookup v ve



{- 
    GENERATE EXPRESSIONS
-}

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

convertExpr :: VarEnv -> Expr -> [TAMInst]
convertExpr ve (LitInteger x) = [LOADL x]
convertExpr ve (DeclaredVar i) =
    case getAddress ve i of
    Nothing -> error ("undeclared variable \"" ++ i ++ "\" in expression")
    Just a  -> [LOAD a]
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



{- 
    GENERATE COMMANDS
-}

-- generate new label based on variable environment state
fresh :: ST Int LabelName
fresh = do n <- stGet
           stUpdate(n+1)
           return ('#' : (show n))

-- generate function for all commands
convertCommand :: VarEnv -> Command -> ST Int [TAMInst]
-- assignment
convertCommand ve (CmdAssign i e) = do
    case getAddress ve i of
        Nothing -> error ("Assignment to undeclared variable \"" ++ i ++ "\"")
        Just a  -> 
            return (
                convertExpr ve e ++
                [STORE a]
            )
-- if condition
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
-- while loop
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
-- getint
convertCommand ve (CmdGetInt i) = do
    case getAddress ve i of
        Nothing -> error ("retrieving undeclared variable \"" ++ i ++ "\"")
        Just a  -> 
            return (
                [GETINT] ++
                [STORE a]
                )
-- printint
convertCommand ve (CmdPrintInt e) =
    return (
        convertExpr ve e ++
        [PUTINT]
        )
-- begin command
convertCommand ve (CmdBegin []) = return []
convertCommand ve (CmdBegin (c:cs)) = do
    com1 <- convertCommand ve c
    com2 <- convertCommand ve (CmdBegin cs)
    return (com1 ++ com2)

-- convert all commands and return only the TAM instructions
commResult :: VarEnv -> Command -> [TAMInst]
commResult ve c =
    let (ts,_) = app (convertCommand ve c) 0
    in ts



{- 
    GENERATE DECLARATION
-}

-- generate declaration TAM code
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

-- generate multiple declarations TAM code
convertDeclarations :: [Declaration] -> ST (VarEnv,StackAddress) [TAMInst]
convertDeclarations [] = return []
convertDeclarations (d:ds) = do
    t <- convertDeclaration d
    ts <- convertDeclarations ds
    return (t ++ ts)

-- wrapper function for declarations generation function
declResult :: [Declaration] -> (VarEnv,[TAMInst])
declResult ds =
    let (ts,(ve,a)) = app (convertDeclarations ds) ([],0)
    -- ([],0) is the initial state!
    in (ve,ts)



{- 
    GENERATE TAM INSTRUCTIONS
-}

-- calling other conversion methods and append 'HALT' at the end of the program
convertAST :: AST -> [TAMInst]
convertAST (Program ds c) =
    let (ve, ts) = declResult ds
    in ts ++ commResult ve c ++ [HALT]


