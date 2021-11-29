{-

Compilers Course (COMP3012), 2021
    Venanzio Capretta
    Nicolai Kraus

Additional Code written by
    Shi Bin Teo
    
-}

{-
    Evaluator for MT

    This is incomplete

    This is too hard to implement as the 
    traversal for AST that contains loops is actually very hard
    compared to how TAM code traverse, simply using counter
-}

module Interpreter where

import Parser
import StateC
import Control.Monad


type VarEnv = [(Identifier, StackAddress)]

type MTInt = Int
type Stack = [MTInt]
type StackAddress = Int

-- type astSavePoint = [AST]

data MTState = MTState {
    msStack :: Stack
} deriving(Eq, Show)

type MTSt a = StateIO MTState a



--INTERPRETER

-- Directly evaluate the input expression

-- eval :: String -> MTInt
-- eval = evaluate . parseMT

-- evaluate :: AST ->  Stack
-- evaluate (Program ds c) = 
-- evaluate _ = error "Fail to Interpret in AST Node: Program"

-- evaluate :: AST -> IO Stack
-- evaluate (Program ds c) = do
--     (stk,_) <- appIO 

{- 
    Interpret Declarations
-}

-- evaluate all declarations and return varenv and stack
-- to understand this simply run this
-- evalDecl [(Var "a" (LitInteger 1)), (EmptyVar "b")]
evalDecl :: [Declaration] -> IO (VarEnv, Stack)
evalDecl ds = do
    (stk, (ve, a)) <- appIO (evalDeclarations [] ds) ([], 0)
    return (ve, stk)

-- evaluate declarations and return state and stack
evalDeclarations :: Stack -> [Declaration] -> StateIO (VarEnv,StackAddress) Stack
evalDeclarations = foldM evalDeclaration
-- evaluate multiple declarations
-- evalDeclarations :: Stack -> [Declaration] -> StateIO (VarEnv,StackAddress) Stack
-- evalDeclarations stk [] = return stk
-- evalDeclarations stk (d:ds) = do
--     stk' <- evalDeclaration stk d
--     evalDeclarations stk' ds

-- evaludate declaration and return state and stack
evalDeclaration :: Stack -> Declaration -> StateIO (VarEnv, StackAddress) Stack
evalDeclaration stk (EmptyVar i) = do
    (ve,a) <- stGetIO
    stUpdateIO ((i,a):ve,a+1)
    return (0:stk)
evalDeclaration stk (Var i e) = do
    (ve,a) <- stGetIO
    stUpdateIO ((i,a):ve,a+1)
    return (evalExpr ve e)


{- 
    Interpret Commands
-}

-- evalComm :: Stack -> VarEnv -> Command -> Stack
-- evalComm stk ve c =
--     let (ts,_) = app (codeGenCommand ve c) 0
--     in ts

evalCommand :: VarEnv -> Command -> MTSt ()
evalCommand ve (CmdAssign i e) = undefined


{- 
    Interpret Expression
-}
evalExpr = undefined


-- -------------------- old Arith Expression Interpreter -------------------- --

-- -- Evaluator for AST
-- evaluate :: Expr -> TAMInt
-- evaluate (LitInteger x)   = x
-- evaluate (BinOp op t1 t2) = binOpEv op (evaluate t1) (evaluate t2)
-- evaluate (UnOp op t)      = unOpEv op (evaluate t)
-- evaluate (TernaryIf b t1 t2) =
--     if evaluate b /= 0
--         then evaluate t1
--         else evaluate t2

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
