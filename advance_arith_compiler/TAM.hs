{-

Compilers Course (COMP3012), 2021
  Venanzio Capretta
  Nicolai Kraus

TAM Virtual Machine

Minimal implementation by Venanzio Capretta.
Supports: Arithmetic expressions, Boolean, Relational, and
Conditional Operators.

-}

module TAM where

import Data.List (intercalate)

type MTInt = Int -- TAM Integer type (values in stack)

-- Instructions of the Virtual Machine

data TAMInst
  = LOADL MTInt   -- push Integer into the stack
  -- Arithmetic operations
  | ADD           -- adds two top values in the stack
  | SUB           -- subtract second element of stack from top
  | MUL           -- multiplies top values in the stack
  | DIV           -- divides the second value by the top (integer division)
  | NEG           -- negates the top of the stack
  -- Boolean operations
  | AND           -- Boolean conjunction (non-zero values are True)
  | OR            -- Boolean disjunction
  | NOT           -- Boolean negation
  -- Relational operations
  | LSS           -- order operation <
  | GTR           -- order operation >
  | EQL           -- equality operator
  deriving (Eq,Show)

type Stack = [MTInt]

emptyStack :: Stack
emptyStack = []

-- Correspondence between Booleans and integers
boolInt :: Bool -> MTInt
boolInt False = 0
boolInt True = 1

-- All non-zero integers correspond to Boolean false
intBool :: MTInt -> Bool
intBool x = x/=0

-- Convenient composition operators

-- Pre-composing with a 2-argument function
infixr 9 .<
(.<) :: (b -> c) -> (a -> a -> b) -> a -> a -> c
g .< f = \ a1 a2 -> g (f a1 a2)

-- Post-composing with a 2-argument function
infixr 9 <.
(<.) :: (b -> b -> c) -> (a -> b) -> a -> a -> c
g <. f = \ a1 a2 -> g (f a1) (f a2)


-- Implementation of boolean operations on Integers, always return 0 or 1

intAND :: MTInt -> MTInt -> MTInt
intAND = boolInt .< (&&) <. intBool

intOR :: MTInt -> MTInt -> MTInt
intOR = boolInt .< (||) <. intBool

intNOT :: MTInt -> MTInt
intNOT = boolInt . not . intBool

-- Relational operations, return 0 (False) or 1 (True)

intLSS :: MTInt -> MTInt -> MTInt
intLSS = boolInt .< (<)

intGTR :: MTInt -> MTInt -> MTInt
intGTR = boolInt .< (>)

intEQL :: MTInt -> MTInt -> MTInt
intEQL = boolInt .< (==)

-- Effect of a single operation on the stack
execute :: Stack -> TAMInst -> Stack
execute stk (LOADL x) = x : stk
-- arithmetic operators
execute (x:y:stk) ADD = y+x : stk
execute (x:y:stk) SUB = y-x : stk
execute (x:y:stk) MUL = y*x : stk
execute (x:y:stk) DIV = y `div` x : stk
execute (x:stk)   NEG = (-x) : stk
-- Boolean operators
execute (x:y:stk) AND = y `intAND` x : stk
execute (x:y:stk) OR = y `intOR` x : stk
execute (x:stk)   NOT = intNOT x : stk
-- relational operators
execute (x:y:stk) LSS = y `intLSS` x : stk
execute (x:y:stk) GTR = y `intGTR` x : stk
execute (x:y:stk) EQL = y `intEQL` x : stk

-- Executing a TAM program (list of instructions)
execTAM :: Stack -> [TAMInst] -> Stack
execTAM = foldl execute

-- Generate the trace of the TAM computation
--   list of pairs of instruction and stack after execution of the instruction
execTrace :: Stack -> [TAMInst] -> [(TAMInst,Stack)]
execTrace stk [] = []
execTrace stk (i:is) =
  let stk' = execute stk i
      trace' = execTrace stk' is
  in ((i,stk'):trace')

-- Printing pairs of value in a two-column table
printTable :: [(String,String)] -> String
printTable pairs = intercalate "\n" $ map (\(a,b) -> fitStr a ++ b) pairs
  where n = maximum (map (length.fst) pairs) + 5
        fitStr a = a ++ replicate (n - length a) ' ' 
                       
-- print the trace of the computation, return the final stack
traceTAM :: Stack -> [TAMInst] -> IO Stack
traceTAM stk tam = do
  let trace = execTrace stk tam
      traceStr = ("Initial stack:", show stk) :
                 map (\(a,b)->(show a,show b)) trace
      finalStk = snd (last trace)
  putStrLn (printTable traceStr)
  return finalStk

-- writing out a TAM program
writeTAM :: [TAMInst] -> String
writeTAM = foldl (\s inst -> s ++ show inst ++ "\n") ""

-- parsing a TAM program
parseTAM :: String -> [TAMInst]
parseTAM = pTAM . words where
  pTAM ("LOADL":x:src) = LOADL (read x) : pTAM src
  pTAM ("ADD":src) = ADD : pTAM src
  pTAM ("SUB":src) = SUB : pTAM src
  pTAM ("MUL":src) = MUL : pTAM src
  pTAM ("DIV":src) = DIV : pTAM src
  pTAM ("NEG":src) = NEG : pTAM src
  pTAM ("AND":src) = AND : pTAM src
  pTAM ("OR" :src) = OR  : pTAM src
  pTAM ("NOT":src) = NOT : pTAM src
  pTAM ("LSS":src) = LSS : pTAM src
  pTAM ("GTR":src) = GTR : pTAM src
  pTAM ("EQL":src) = EQL : pTAM src
  pTAM _ = []
