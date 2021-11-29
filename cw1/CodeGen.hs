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

module CodeGen where

import Parser
    ( BinOperator(..),
      UnOperator(..),
      Expr(..),
      Command(..),
      Declaration(..),
      AST(..),
      Identifier )
import TAM ( LabelName, StackAddress, TAMInst(..) )
import StateC ( app, stGet, stUpdate, ST )



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

codeGenExpr :: VarEnv -> Expr -> [TAMInst]
codeGenExpr ve (LitInteger x) = [LOADL x]
codeGenExpr ve (DeclaredVar i) =
    case getAddress ve i of
    Nothing -> error ("undeclared variable \"" ++ i ++ "\" in expression")
    Just a  -> [LOAD a]
-- Relational operators that don't have TAM instructions
codeGenExpr ve (BinOp LeqOp t1 t2) =
    codeGenExpr ve (BinOp Disjunction (BinOp LssOp t1 t2) (BinOp EqOp t1 t2))
codeGenExpr ve (BinOp GeqOp t1 t2) =
    codeGenExpr ve (BinOp Disjunction (BinOp GtrOp t1 t2) (BinOp EqOp t1 t2))
codeGenExpr ve (BinOp NeqOp t1 t2) =
    codeGenExpr ve (UnOp NegBool (BinOp EqOp t1 t2))
-- Conditional expressions (double negation to normalize the Boolean value)
--   b ? e1 : e2  ~  (!!b) * e1 + (!b) * e2
codeGenExpr ve (TernaryIf b t1 t2) =
    codeGenExpr ve (BinOp Addition
               (BinOp Multiplication (UnOp NegBool (UnOp NegBool b)) t1)
               (BinOp Multiplication (UnOp NegBool b) t2)
            )
-- General cases
codeGenExpr ve (BinOp op t1 t2) =
    (codeGenExpr ve t1 ++ codeGenExpr ve t2 ++ [binOpTAM op])
codeGenExpr ve (UnOp op t) =
    (codeGenExpr ve t ++ [unOpTAM op])



{- 
    GENERATE COMMANDS
-}

-- generate new label based on variable environment state
fresh :: ST Int LabelName
fresh = do n <- stGet
           stUpdate(n+1)
           return ('#' : (show n))

-- generate function for all commands
codeGenCommand :: VarEnv -> Command -> ST Int [TAMInst]
-- assignment
codeGenCommand ve (CmdAssign i e) = do
    case getAddress ve i of
        Nothing -> error ("Assignment to undeclared variable \"" ++ i ++ "\"")
        Just a  -> 
            return (
                codeGenExpr ve e ++
                [STORE a]
            )
-- if condition
codeGenCommand ve (CmdIf e c1 c2) = do
    l1 <- fresh
    l2 <- fresh
    com1 <- codeGenCommand ve c1
    com2 <- codeGenCommand ve c2
    return (
        codeGenExpr ve e ++
        [JUMPIFZ l1] ++
        com1 ++
        [JUMP l2] ++
        [Label l1] ++
        com2 ++
        [Label l2]
        )
-- while loop
codeGenCommand ve (CmdWhile e c) = do
    l1 <- fresh
    l2 <- fresh
    com1 <- codeGenCommand ve c
    return (
        [Label l1] ++
        codeGenExpr ve e ++
        [JUMPIFZ l2] ++
        com1 ++
        [JUMP l1] ++
        [Label l2]
        )
-- getint
codeGenCommand ve (CmdGetInt i) = do
    case getAddress ve i of
        Nothing -> error ("retrieving undeclared variable \"" ++ i ++ "\"")
        Just a  -> 
            return (
                [GETINT] ++
                [STORE a]
                )
-- printint
codeGenCommand ve (CmdPrintInt e) =
    return (
        codeGenExpr ve e ++
        [PUTINT]
        )
-- begin command
codeGenCommand ve (CmdBegin []) = return []
codeGenCommand ve (CmdBegin (c:cs)) = do
    com1 <- codeGenCommand ve c
    com2 <- codeGenCommand ve (CmdBegin cs)
    return (com1 ++ com2)

-- codeGen all commands and return only the TAM instructions
commResult :: VarEnv -> Command -> [TAMInst]
commResult ve c =
    let (ts,_) = app (codeGenCommand ve c) 0
    in ts



{- 
    GENERATE DECLARATION
-}

-- generate declaration TAM code
codeGenDeclaration :: Declaration -> ST (VarEnv,StackAddress) [TAMInst]
codeGenDeclaration (EmptyVar i) = do
    (ve,a) <- stGet
    stUpdate ((i,a):ve,a+1)
    return
        [LOADL 0]
codeGenDeclaration (Var i e) = do
    (ve,a) <- stGet
    stUpdate ((i,a):ve,a+1)
    return (codeGenExpr ve e)

-- generate multiple declarations TAM code
codeGenDeclarations :: [Declaration] -> ST (VarEnv,StackAddress) [TAMInst]
codeGenDeclarations [] = return []
codeGenDeclarations (d:ds) = do
    t <- codeGenDeclaration d
    ts <- codeGenDeclarations ds
    return (t ++ ts)

-- wrapper function for declarations generation function
declResult :: [Declaration] -> (VarEnv,[TAMInst])
declResult ds =
    let (ts,(ve,a)) = app (codeGenDeclarations ds) ([],0)
    -- ([],0) is the initial state!
    in (ve,ts)



{- 
    GENERATE TAM INSTRUCTIONS
-}

-- calling other conversion methods and append 'HALT' at the end of the program
codeGenAST :: AST -> [TAMInst]
codeGenAST (Program ds c) =
    let (ve, ts) = declResult ds
    in ts ++ commResult ve c ++ [HALT]


