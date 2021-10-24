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

executeAll :: [TamInst] -> Stack
executeAll tp = execute tp []

execute :: [TamInst] -> Stack -> Stack
execute [] s = s
execute (LOADL n: tp) s = execute tp (n:s)
execute (NEG:tp) (n:s) = execute tp (-n:s)
execute (op:tp) (a:b:s) = execute tp (absOptoConcrOp op b a : s)

absOptoConcrOp :: TamInst -> Int -> Int -> Int
absOptoConcrOp ADD = (+)
absOptoConcrOp SUB = (-)
absOptoConcrOp MUL = (*)
absOptoConcrOp DIV = div


