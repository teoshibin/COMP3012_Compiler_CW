
{-

Compilers Course (COMP3012), 2021
    Venanzio Capretta
    Nicolai Kraus

Additional Code written by
    Shi Bin Teo

-}

{- 
    TAM Virtual Machine

    Definition of TAM language
    Execution of TAM language
    Parser for TAM language
-}

module TAM where

import Data.List (intercalate)
import StateC ( appIO, liftIO, stGetIO, stUpdateIO, StateIO )
import GlobalFunc ( prompt )



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


-- ----------------- STATE AUXILIARY FUNCTION FOR EXECUTION ----------------- --

-- function with postfix T are all state transformer for TAMSt

{- 
    TAMProgram AUXILIARY FUNCTIONS
-}

-- get program from state
progGetT :: TAMSt [TAMInst]
progGetT = do
    ts <- stGetIO
    return (tsProgram ts)

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
    Stack AUXILIARY FUNCTIONS
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

-- value retrieval from stack within state via indexing
stkReadT :: StackAddress -> TAMSt TAMInt
stkReadT a = do
    stk <- stkGetT
    let n = reverse stk !! a
    return n

-- helper function for stkWriteT
-- value assignment via indexing
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i+1) xs

-- value assignment to stack within state via indexing
stkWriteT :: StackAddress -> TAMInt -> TAMSt ()
stkWriteT a n = do
    stk <- stkGetT
    let revStk = reverse stk
        revStk' = replaceAtIndex a n revStk
        stk' = reverse revStk'
    stkUpdateT stk'



{- 
    Counter AUXILIARY FUNCTIONS
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

-- increament the counter by 1
stepCounterT :: TAMSt ()
stepCounterT = do
    ctr <- ctrGetT
    ctrUpdateT (ctr + 1)

-- helper function for lCounter
-- find instruction that match in instruction list
findInst :: TAMInst -> TAMProgram -> Maybe Counter
findInst inst prog =
    let index = length (takeWhile (/= inst) prog)
    in if index > (length prog - 1) then Nothing else Just index

-- helper function for findLabelT
-- look for an element in instruction list that match the LabelName and return it's Counter
lCounter :: LabelName -> TAMProgram -> Counter
lCounter l prog =
    case findInst (Label l) prog of
        Just c -> c
        Nothing -> error ("Label \"" ++ l ++ "\" not found")

-- find input label within the program and return its counter position
findLabelT :: LabelName -> TAMSt Counter
findLabelT l = do
    prog <- progGetT
    return (lCounter l prog)



-- -------------------------------- EXECUTION ------------------------------- --

{- 
    EXECUTION AUXILIARY FUNCTIONS
-}

-- apply f to 2 pop values then push it
cal1valueT :: (TAMInt -> TAMInt) -> TAMSt ()
cal1valueT f = do
    x <- popT
    pushT (f x)
    stepCounterT

-- apply f to 1 pop values then push it
cal2valueT :: (TAMInt -> TAMInt -> TAMInt) -> TAMSt ()
cal2valueT f = do
    b <- popT
    a <- popT
    pushT (f a b)
    stepCounterT

-- Correspondence between Booleans and integers
boolInt :: Bool -> TAMInt
boolInt False = 0
boolInt True = 1

-- All non-zero integers correspond to Boolean false
intBool :: TAMInt -> Bool
intBool x = x/=0

-- Composition operators

-- Pre-composing with a 2-argument function
infixr 9 .<
(.<) :: (b -> c) -> (a -> a -> b) -> a -> a -> c
g .< f = \ a1 a2 -> g (f a1 a2)

-- Post-composing with a 2-argument function
infixr 9 <.
(<.) :: (b -> b -> c) -> (a -> b) -> a -> a -> c
g <. f = \ a1 a2 -> g (f a1) (f a2)

-- Implementation of boolean operations on Integers, always return 0 or 1

intAND :: TAMInt -> TAMInt -> TAMInt
intAND = boolInt .< (&&) <. intBool

intOR :: TAMInt -> TAMInt -> TAMInt
intOR = boolInt .< (||) <. intBool

intNOT :: TAMInt -> TAMInt
intNOT = boolInt . not . intBool

-- Relational operations, return 0 (False) or 1 (True)

intLSS :: TAMInt -> TAMInt -> TAMInt
intLSS = boolInt .< (<)

intGTR :: TAMInt -> TAMInt -> TAMInt
intGTR = boolInt .< (>)

intEQL :: TAMInt -> TAMInt -> TAMInt
intEQL = boolInt .< (==)


{- 
    TAM EXECUTION 
-}

-- execute one line of TAM code
executeOne :: TAMInst -> TAMSt ()
-- stack operations
executeOne (LOADL n) = do
    pushT n
    stepCounterT
executeOne (LOAD a) = do
    n <- stkReadT a
    pushT n
    stepCounterT
executeOne (STORE a) = do
    n <- popT
    stkWriteT a n
    stepCounterT
executeOne GETINT = do
    s <- liftIO (prompt "input : ")
    pushT (read s)
    stepCounterT
executeOne PUTINT = do
    x <- popT
    liftIO (putStrLn ("output > " ++ show x))
    stepCounterT
-- flow control
executeOne (JUMP l) = do
    c <- findLabelT l
    ctrUpdateT c
executeOne (JUMPIFZ l) = do
    x <- popT
    if x == 0 then do
        c <- findLabelT l
        ctrUpdateT c
    else
        stepCounterT
executeOne (Label _) = stepCounterT
executeOne HALT = return ()
-- arithmetic operations
executeOne ADD = cal2valueT (+)
executeOne SUB = cal2valueT (-)
executeOne MUL = cal2valueT (*)
executeOne DIV = cal2valueT div
executeOne NEG = cal1valueT (*(-1))
-- boolean operations
executeOne AND = cal2valueT intAND
executeOne OR = cal2valueT intOR
executeOne NOT = cal1valueT intNOT
-- relational operations
executeOne LSS = cal2valueT intLSS
executeOne GTR = cal2valueT intGTR
executeOne EQL = cal2valueT intEQL

-- execute many lines of TAM
executeMany :: TAMSt Stack
executeMany = do
    inst <- pointedInstT
    executeOne inst
    if inst == HALT
        then stkGetT
        else executeMany

-- execute many lines of TAM and return only the stack
executeTAM :: [TAMInst] -> IO Stack
executeTAM tam = do
    (stk,_) <- appIO executeMany (initTS tam)
    return stk



-- -------------------------- Stack Tracing Execute ------------------------- --

-- Generate the trace of the TAM computation
--   list of pairs of instruction and stack after execution of the instruction
-- This is equivalent of executeMany but save every single state as a list and return it at the end
execTrace :: [(TAMInst,Stack)] -> TAMSt [(TAMInst,Stack)]
execTrace xs = do
    inst <- pointedInstT
    executeOne inst
    stk <- stkGetT
    let xs' = (inst, stk):xs
    if inst == HALT then do
        return xs'
    else
        execTrace xs'

-- Printing pairs of value in a two-column table
printTable :: [(String,String)] -> String
printTable pairs = 
    intercalate "\n" $ map (\(a,b) -> fitStr a ++ b) pairs
        where   n = maximum (map (length.fst) pairs) + 5
                fitStr a = a ++ replicate (n - length a) ' '

-- execute many lines of TAM and return only the stack
-- equivalent of executeTAM
traceTAM :: [TAMInst] -> IO Stack
traceTAM tam = do
    (trace, _) <- appIO (execTrace []) (initTS tam)
    let trace' = reverse trace
        traceStr = 
            ("Initial stack:","[]") : 
            map (\(a,b)->(show a,show b)) trace'
        finalStk = snd (last trace')
    putStrLn ""
    putStrLn (printTable traceStr)
    return finalStk



-- -------------------------------- PARSE TAM ------------------------------- --

unQuote :: String -> LabelName
unQuote = filter (/='"')

pTAM :: [String] -> [TAMInst]
-- stack operations
pTAM ("LOADL"   :x:src) = LOADL      (read x): pTAM src
pTAM ("LOAD"    :x:src) = LOAD       (read x): pTAM src
pTAM ("STORE"   :x:src) = STORE      (read x): pTAM src
pTAM ("GETINT"    :src) = GETINT             : pTAM src
pTAM ("PUTINT"    :src) = PUTINT             : pTAM src
-- flow control
pTAM ("JUMP"    :x:src) = JUMP    (unQuote x): pTAM src
pTAM ("JUMPIFZ" :x:src) = JUMPIFZ (unQuote x): pTAM src
pTAM ("Label"   :x:src) = Label   (unQuote x): pTAM src
pTAM ("HALT"      : _ ) = [HALT]
-- arithmetic operations
pTAM ("ADD"       :src) = ADD                : pTAM src
pTAM ("SUB"       :src) = SUB                : pTAM src
pTAM ("MUL"       :src) = MUL                : pTAM src
pTAM ("DIV"       :src) = DIV                : pTAM src
pTAM ("NEG"       :src) = NEG                : pTAM src
-- boolean operations
pTAM ("AND"       :src) = AND                : pTAM src
pTAM ("OR"        :src) = OR                 : pTAM src
pTAM ("NOT"       :src) = NOT                : pTAM src
-- relational operations
pTAM ("LSS"       :src) = LSS                : pTAM src
pTAM ("GTR"       :src) = GTR                : pTAM src
pTAM ("EQL"       :src) = EQL                : pTAM src

-- parsing a TAM program
parseTAM :: String -> [TAMInst]
parseTAM = pTAM . words

-- writing out a TAM program
tam2String :: [TAMInst] -> String
tam2String = foldl (\s inst -> s ++ show inst ++ "\n") ""
