module TAM where

{-
executeTAM [LOADL 5, LOADL 8, LOADL 7, ADD, LOADL 2, SUB, LOADL 4, DIV, MUL]
= [15]
-}

data TamInst =
    LOADL Int
    | ADD
    | SUB
    | MUL
    | DIV
    | NEG
    deriving (Eq, Show)

type Stack = [Int]

executeTAM :: [TamInst] -> Stack
executeTAM = foldl (flip execute) []

execute :: TamInst -> Stack -> Stack
execute (LOADL n) s = n:s
execute NEG (n:s) = -n:s
execute ADD (a:b:s) = b + a : s
execute SUB (a:b:s) = b - a : s
execute MUL (a:b:s) = b * a : s
execute DIV (a:b:s) = b `div` a : s


