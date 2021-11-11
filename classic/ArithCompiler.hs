module ArithCompiler where
import TAM
import ArithParser

expCode :: AST -> [TamInst]
expCode (LitInteger n) = [LOADL n]
expCode (BinOp op a b) = expCode a ++ expCode b ++ [binOpTAM op]
expCode (UnOp op a) = expCode a ++ [unOpTAM op]

binOpTAM :: BinOperator -> TamInst
binOpTAM Addition = ADD
binOpTAM Subtraction = SUB
binOpTAM Multiplication = MUL
binOpTAM Division = DIV

unOpTAM :: UnOperator -> TamInst
unOpTAM Negation = NEG

compile :: String -> [TamInst]
compile = expCode . parse

compileRun :: String -> Stack
compileRun = executeTAM . compile