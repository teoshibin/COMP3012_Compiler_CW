
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
import StateC

-- ----------------------------- Data Structure ----------------------------- --

{- 
    TAM LANGUAGE DEFINITION
-}

type TAMInt = Int           -- TAM Integer type
type StackAddress = Int     -- TAM StackAddress
type LabelName = String     -- TAM Label
data TAMInst
    -- stack operations
    = LOADL TAMInt          -- push value on to the Stack
    | LOAD StackAddress     -- copy value from StackAddress on to the stack
    | STORE StackAddress    -- pop stack and store to StackAddress
    | GETINT                -- read value from terminal push on to the stack
    | PUTINT                -- pop stack and print pop value
    -- flow control
    | JUMP LabelName        -- unconditional jump
    | JUMPIFZ LabelName     -- pop stack and jump if the pop value is 0
    | Label LabelName       -- Label for jumping
    | HALT                  -- stop execution     
    -- Arithmetic operations
    | ADD                   -- adds two top values in the stack
    | SUB                   -- subtract second element of stack from top
    | MUL                   -- multiplies top values in the stack
    | DIV                   -- divides the second value by the top (integer division)
    | NEG                   -- negates the top of the stack
    -- Boolean operations
    | AND                   -- Boolean conjunction (non-zero values are True)
    | OR                    -- Boolean disjunction
    | NOT                   -- Boolean negation
    -- Relational operations
    | LSS                   -- order operation <
    | GTR                   -- order operation >
    | EQL                   -- equality operator
    deriving (Eq,Show)


{- 
    TAM EXECUTION STATE DEFINITION
-}

type Stack = [TAMInt]
type TAMProgram = [TAMInst]
type Counter = Int
data TAMState = TAMState {
    tsProgram :: TAMProgram,
    tsCounter :: Counter,
    tsStack :: Stack
} deriving(Eq, Show)
-- TAMState as State Transformer State
type TAMSt a = StateIO TAMState a


-- ------------------------ STATE AUXILIARY FUNCTIONS ----------------------- --

{- 
    STATE AUXILIARY FUNCTIONS FOR TAMProgram
-}

-- get program from state
progGetT :: TAMSt [TAMInst]
progGetT = do
    ts <- stGetIO
    return (tsProgram ts)


{- 
    STATE AUXILIARY FUNCTIONS FOR Stack
-}

-- get stack from state
stkGetT :: TAMSt Stack
stkGetT = do
    ts <- stGetIO
    return (tsStack ts)

-- replace stack within the state with input stack
stkUpdateT :: Stack -> TAMSt ()
stkUpdateT stk = do
    ts <- stGetIO
    stUpdateIO (ts {tsStack = stk})


{- 
    STATE AUXILIARY FUNCTIONS FOR Counter
-}

-- get counter from state
ctrGetT :: TAMSt Counter
ctrGetT = do
    ts <- stGetIO
    return (tsCounter ts)

-- replace counter within state with input counter
ctrUpdateT :: Counter -> TAMSt ()
ctrUpdateT c = do
    ts <- stGetIO
    stUpdateIO (ts {tsCounter = c})


-- ------------------------------- Operations ------------------------------- --

{- 
    PROGRAM OPERATIONS
-}

-- initialize a empty stateIO contianing the program
initTS :: TAMProgram -> TAMState
initTS tam = TAMState {tsProgram = tam, tsCounter = 0, tsStack = []}

-- get the counter pointing instruction
pointedInstT :: TAMSt TAMInst
pointedInstT = do
    prog <- progGetT
    ctr <- ctrGetT
    return (prog !! ctr)


{- 
    STACK OPERATIONS
-}

-- pop stack and return a value
popT :: TAMSt TAMInt
popT = do
    stk <- stkGetT
    stkUpdateT (tail stk)
    return (head stk)

-- push an input value to stack
pushT :: TAMInt -> TAMSt ()
pushT n = do
    stk <- stkGetT
    stkUpdateT (n:stk)


{- 
    COUNTER OPERATIONS
-}

-- increament the counter by 1
stepCounterT :: TAMSt ()
stepCounterT = do
    ctr <- ctrGetT
    ctrUpdateT (ctr + 1)

-- find input label within the program and return its counter position
findLabelT :: LabelName -> TAMSt Counter
findLabelT l = do
    prog <- progGetT
    return (lCounter l prog)


{- 
    COUNTER HELPER FUNCTIONS
-}

-- TODO look for an element in TAMProgram that match the LabelName and return it's Counter
lCounter :: LabelName -> TAMProgram -> Counter
lCounter l prog = 
    case findLabelIndex l prog 0 of
        Just c -> c
        Nothing -> error ("Label \"" ++ l ++ "\" not found")

findLabelIndex :: LabelName -> TAMProgram -> Counter -> Maybe Counter
findLabelIndex l ((Label l'):ps) c
  | l == l' = Just c
  | null ps = Nothing
  | otherwise = findLabelIndex l ps (c+1)
findLabelIndex l ps c = findLabelIndex l ps (c+1)



{- 
    TODO EXECUTION OF TAM
    NOTE writing and reading to or from the stack 
         CAVEAT: the StackAddress 0 or the oldest value is stored at the bottom of the stack
-}

-- execute one line of TAM code
executeOne :: TAMInst -> TAMSt ()
executeOne HALT = return ()
executeOne (LOADL n) = do
    pushT n
    stepCounterT
executeOne (JUMP l) = do
    c <- findLabelT l
    ctrUpdateT c
executeOne GETINT = do
    liftIO (putStrLn "input : ")
    s <- liftIO getLine
    pushT (read s)
    stepCounterT
executeOne PUTINT = do
    x <- popT
    liftIO (putStrLn ("output > " ++ show x))
    stepCounterT

executeMany :: TAMSt Stack
executeMany = do
    inst <- pointedInstT
    executeOne inst
    if inst == HALT
        then stkGetT
        else executeMany


exectTAM :: [TAMInst] -> IO Stack
exectTAM tam = do
    (stk,_) <- appIO executeMany (initTS tam)
    return stk

-- execute :: TAMInst -> TAMSt ()
-- execute HALT = return()
-- execute (STORE a) = undefined 
-- execute (PUTINT a) = undefined 

-- execute :: TAMInst -> TAMState -> TAMState
-- exeStep :: TAMState -> TAMState


{- 
    TODO EXECUTION FOR EXPRESSION
-}

-- -- Correspondence between Booleans and integers
-- boolInt :: Bool -> TAMInt
-- boolInt False = 0
-- boolInt True = 1

-- -- All non-zero integers correspond to Boolean false
-- intBool :: TAMInt -> Bool
-- intBool x = x/=0

-- -- Convenient composition operators

-- -- Pre-composing with a 2-argument function
-- infixr 9 .<
-- (.<) :: (b -> c) -> (a -> a -> b) -> a -> a -> c
-- g .< f = \ a1 a2 -> g (f a1 a2)

-- -- Post-composing with a 2-argument function
-- infixr 9 <.
-- (<.) :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- g <. f = \ a1 a2 -> g (f a1) (f a2)


-- -- Implementation of boolean operations on Integers, always return 0 or 1

-- intAND :: TAMInt -> TAMInt -> TAMInt
-- intAND = boolInt .< (&&) <. intBool

-- intOR :: TAMInt -> TAMInt -> TAMInt
-- intOR = boolInt .< (||) <. intBool

-- intNOT :: TAMInt -> TAMInt
-- intNOT = boolInt . not . intBool

-- -- Relational operations, return 0 (False) or 1 (True)

-- intLSS :: TAMInt -> TAMInt -> TAMInt
-- intLSS = boolInt .< (<)

-- intGTR :: TAMInt -> TAMInt -> TAMInt
-- intGTR = boolInt .< (>)

-- intEQL :: TAMInt -> TAMInt -> TAMInt
-- intEQL = boolInt .< (==)

-- -- Effect of a single operation on the stack
-- execute :: Stack -> TAMInst -> Stack
-- execute stk (LOADL x) = x : stk
-- -- arithmetic operators
-- execute (x:y:stk) ADD = y+x : stk
-- execute (x:y:stk) SUB = y-x : stk
-- execute (x:y:stk) MUL = y*x : stk
-- execute (x:y:stk) DIV = y `div` x : stk
-- execute (x:stk)   NEG = (-x) : stk
-- -- Boolean operators
-- execute (x:y:stk) AND = y `intAND` x : stk
-- execute (x:y:stk) OR = y `intOR` x : stk
-- execute (x:stk)   NOT = intNOT x : stk
-- -- relational operators
-- execute (x:y:stk) LSS = y `intLSS` x : stk
-- execute (x:y:stk) GTR = y `intGTR` x : stk
-- execute (x:y:stk) EQL = y `intEQL` x : stk

-- -- Executing a TAM program (list of instructions)
-- execTAM :: Stack -> [TAMInst] -> Stack
-- execTAM = foldl execute

-- -- Generate the trace of the TAM computation
-- --   list of pairs of instruction and stack after execution of the instruction
-- execTrace :: Stack -> [TAMInst] -> [(TAMInst,Stack)]
-- execTrace stk [] = []
-- execTrace stk (i:is) =
--     let stk' = execute stk i
--         trace' = execTrace stk' is
--     in ((i,stk'):trace')

-- -- Printing pairs of value in a two-column table
-- printTable :: [(String,String)] -> String
-- printTable pairs = 
--     intercalate "\n" $ map (\(a,b) -> fitStr a ++ b) pairs
--         where   n = maximum (map (length.fst) pairs) + 5
--                 fitStr a = a ++ replicate (n - length a) ' '

-- -- print the trace of the computation, return the final stack
-- traceTAM :: Stack -> [TAMInst] -> IO Stack
-- traceTAM stk tam = do
--     let trace = execTrace stk tam
--         traceStr = ("Initial stack:", show stk) :
--                     map (\(a,b)->(show a,show b)) trace
--         finalStk = snd (last trace)
--     putStrLn (printTable traceStr)
--     return finalStk

-- writing out a TAM program
writeTAM :: [TAMInst] -> String
writeTAM = foldl (\s inst -> s ++ show inst ++ "\n") ""

-- -- parsing a TAM program
-- parseTAM :: String -> [TAMInst]
-- parseTAM = pTAM . words where
--     pTAM ("LOADL":x:src) = LOADL (read x) : pTAM src
--     pTAM ("ADD":src) = ADD : pTAM src
--     pTAM ("SUB":src) = SUB : pTAM src
--     pTAM ("MUL":src) = MUL : pTAM src
--     pTAM ("DIV":src) = DIV : pTAM src
--     pTAM ("NEG":src) = NEG : pTAM src
--     pTAM ("AND":src) = AND : pTAM src
--     pTAM ("OR" :src) = OR  : pTAM src
--     pTAM ("NOT":src) = NOT : pTAM src
--     pTAM ("LSS":src) = LSS : pTAM src
--     pTAM ("GTR":src) = GTR : pTAM src
--     pTAM ("EQL":src) = EQL : pTAM src
--     pTAM _ = []
