{-
Compilers (COMP3012)
University of Nottingham, 22 Oct 2021
Nicolai Kraus (replacing Venanzio Capretta)

TAM (Triangle Abstract Machine)
-}

module TAM where

-- a TAM program is a sequence of the following very
-- simple instructions:

data TamInst
  = LOADL Int
  | ADD
  | SUB
  | MUL
  | DIV
  | NEG
  deriving (Eq, Show)

-- a TAM program operates on a stack, which can be
-- implemented as a simple list:
type Stack = [Int]

-- In the lecture, I have defined
--    execute :: [TamInst] -> Stack -> Stack
-- This works, but to make it a bit more modular, we
-- can do a single step:

execute :: Stack -> TamInst -> Stack
execute s (LOADL n) = n : s
execute (a : s) NEG = (-a) : s
execute (a : b : s) op = (absOpToConcrOp op b a) : s

-- converting an abstract (TAM) operation into a
-- concrete binary operation on Int:
absOpToConcrOp :: TamInst -> Int -> Int -> Int
absOpToConcrOp ADD = (+)
absOpToConcrOp SUB = (-)
absOpToConcrOp MUL = (*)
absOpToConcrOp DIV = div

-- executing a sequence of TAM instructions:
execPrgm :: Stack -> [TamInst] -> Stack
execPrgm = foldl execute

{- Recall the definition of foldl. The above is
equivalent to:

execPrgm s [] = s
execPrgm s (op : ops) = execPrgm (execute s op) ops
-}
