module TAM where

data TamInst =
    LOADL Int
    | ADD
    | SUB
    | MUL
    | DIV
    | NEG
    deriving (Eq, Show)

type Stack = [Int]

execute :: [TamInst] -> Stack -> Stack
execute [] s = s
execute (LOADL n: tp) s = execute tp (n:s)
execute (ADD : tp) (a:b:s) = execute tp ((a+b):s)
execute (op:tp) (a:b:s) = execute tp (absOptoConcrOp op b a : s)

absOptoConcrOp :: TamInst -> Int -> Int -> Int
absOptoConcrOp ADD = (+)
absOptoConcrOp SUB = (-)
absOptoConcrOp MUL = (*)
absOptoConcrOp DIV = div


